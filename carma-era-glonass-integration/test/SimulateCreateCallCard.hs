{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns, LambdaCase, ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

-- For Servant's stuff
{-# LANGUAGE DataKinds, TypeOperators #-}

module Main (main) where

import           Test.Hspec

import           Data.Proxy
import           Data.Maybe (isJust)
import qualified Data.Vector as V
import           Text.InterpolatedString.QM
import qualified Data.Configurator as Conf
import           Data.Aeson
import qualified Data.HashMap.Lazy as HM
import           Data.Foldable (toList)

import           Control.Monad
import           Control.Monad.Catch
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception (throw)
import           Control.Monad.Random.Class (getRandomRs)

import           System.Environment

import           Database.Persist.Sql (toSqlKey)

import qualified Network.Wai.Handler.Warp as Warp
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import           Servant
import           Servant.Client

import           Carma.Utils.Operators
import           Carma.Model.Case.Persistent
import           Carma.EraGlonass.Test.Helpers
import           Carma.EraGlonass.Test.Types.EGCreateCallCardRequest (testData)
import           Carma.EraGlonass.Model.CaseEraGlonassFailure.Types


type FailuresAPI
    =  -- GET /debug/failures/count.json
       "count.json" :> Get '[JSON] Word

  :<|> -- GET /debug/failures/list.json?limit=10
       "list.json" :> QueryParam "limit" Word :> Get '[JSON] Value


type ServerAPI
    =  -- Just JSON-ny representation of EG.CRM.01,
       -- without strong typing for testing purpuses,
       -- to make sure we could handle provided JSON example.
       --
       -- POST /calls/status
       --
       "calls" :> "status" :> ReqBody '[JSON] Value
                           :> Post    '[JSON] Value

  :<|> "debug" :> (    "failures" :> FailuresAPI

                  :<|> -- GET /debug/background-tasks/count.json
                       "background-tasks" :> "count.json" :> Get '[JSON] Word

                  :<|> -- GET /debug/case/:caseid/get.json
                       "case"
                       :> Capture "caseid" CaseId
                       :> "get.json"
                       :> Get '[JSON] Value
                  )


main :: IO ()
main = do
  serverLock <- newMVar ()

  -- Run with @CARMA_EG_TEST_WITHOUT_TESTING_SERVER=Y@ environment variable to
  -- use separately started server (prevents starting testing server
  -- automatically for each test). This feature just for debugging purposes
  -- during development.
  withoutTestingServer <-
    lookupEnv "CARMA_EG_TEST_WITHOUT_TESTING_SERVER" <&> (== Just "Y")

  hspec $
    describe "EG.CRM.01" $ egCRM01 withoutTestingServer serverLock


egCRM01 :: Bool -> MVar () -> Spec
egCRM01 withoutTestingServer serverLock =
  describe "Simulating creating Call Card by Era Glonass" $ do

    let !testData' = either error id testData

    it "Usual successful creating EG Call Card" $
      withTestingServer withoutTestingServer serverLock $ do
        result <- getRequestMaker >>= \f -> f $ createCallCard testData'
        result `shouldSatisfy` \case
          Right (Object kv) -> let

            hasCaseId = HM.member "cardidProvider" kv
            -- ^ Only successful constructor have this field

            in HM.lookup "acceptCode" kv == Just (String "OK") && hasCaseId

          _ -> False

    describe "Dictionary city fields of a case are filled by gis data" $ do

      let obtainCase = \case
            Right (Object kv) ->
              case HM.lookup k kv of
                   Just (String textCaseId) ->
                     pure $ toSqlKey $ read [qm| {textCaseId} |]
                   Just x -> fail [qm| "{k}" key is not a "String": {x} |]
                   Nothing -> fail [qm| "{k}" key not found! |]

            Right x -> fail [qm| Root is not an "Object": {x} |]
            Left err -> throwM err

            where k = "cardidProvider"

      let cityFieldsAreFilledPredicate = \case
            Right (Object kv) -> let

              firstField =
                case HM.lookup "caseAddress_city" kv of
                     Just (Number x) -> Just x
                     _ -> Nothing

              secondField =
                case HM.lookup "city" kv of
                     Just (Number x) -> Just x
                     _ -> Nothing

              in isJust firstField
              && isJust secondField
              && firstField == secondField

            _ -> False

      let cityFieldsAreNotFilledPredicate = \case
            Right (Object kv) -> let

              firstField =
                case HM.lookup "caseAddress_city" kv of
                     Just Null -> True
                     _ -> False

              secondField =
                case HM.lookup "city" kv of
                     Just Null -> True
                     _ -> False

              in firstField && secondField

            _ -> False

      it "Found city by its label" $
        withTestingServer withoutTestingServer serverLock $ do
          result <- getRequestMaker >>= \f -> f $ createCallCard testData'
          caseId <- obtainCase result
          caseData <- getRequestMaker >>= \f -> f $ getCase caseId
          caseData `shouldSatisfy` cityFieldsAreFilledPredicate
          caseData `shouldNotSatisfy` cityFieldsAreNotFilledPredicate

      it "City with such label not exists" $ do
        -- Random VIN to avoid merging with already exiting @Case@
        randomVin <-
          getRandomRs ('A', 'Z')
            <&> take 17
            <&> map (\case 'I' -> '0'
                           'O' -> '1'
                           'Q' -> '2'
                           x -> x)

        withTestingServer withoutTestingServer serverLock $ do
          result <- getRequestMaker >>= \f -> f $ createCallCard $ let

            -- | Modifier of "gis.settlementName" key to prevent city
            -- dictionary __@Case@__'s field from detecting.
            fGisList :: Object -> Either String Value
            fGisList kv = case HM.lookup k kv of
              Just (Array (V.toList -> (Object x : xs))) -> do
                newGis <- fGisItem x
                Right $ Object $
                  HM.insert k (Array $ V.fromList (newGis : xs)) kv
              Just x ->
                Left [qm| "{k}" key is not an "Array" of "Object"s: {x} |]
              Nothing ->
                Left [qm| "{k}" key not found in hash-map: {kv} |]
              where k = "gis"

            fGisItem :: Object -> Either String Value
            fGisItem kv = case HM.lookup k kv of
              Just (String _) ->
                Right $ Object $
                  HM.insert k (String "unknown city label plug") kv
              Just x ->
                Left [qm| "{k}" key is not a "String": {x} |]
              Nothing ->
                Left [qm| "{k}" key not found in hash-map: {kv} |]
              where k = "settlementName"

            -- | "vehicle.vin" key modifier to prevent from merging with already
            -- existing __@Case@__.
            fVehicle :: Object -> Either String Value
            fVehicle kv = case HM.lookup k kv of
              Just (Object kv') -> do
                let k' = "vin"

                newVehicle <-
                  case HM.lookup k' kv' of
                       Just (String _) ->
                         Right $ HM.insert k' (String [qm| {randomVin} |]) kv'
                       Just x ->
                         Left [qm| "{k'}" key is not a "String": {x} |]
                       Nothing ->
                         Left [qm| "{k'}" key not found in hash-map: {kv'} |]

                Right $ Object $ HM.insert k (Object newVehicle) kv

              Just x ->
                Left [qm| "{k}" key is not an "Object": {x} |]
              Nothing ->
                Left [qm| "{k}" key not found in hash-map: {kv} |]

              where k = "vehicle"

            rootF :: Value -> Either String Value
            rootF (Object x) =
              fGisList x >>= \case
                Object y -> fVehicle y
                y -> Left [qm| Root element is not an "Object": {y} |]
            rootF x = Left [qm| Root element is not an "Object": {x} |]

            in either error id $ testData >>= rootF

          caseId <- obtainCase result
          caseData <- getRequestMaker >>= \f -> f $ getCase caseId
          caseData `shouldNotSatisfy` cityFieldsAreFilledPredicate
          caseData `shouldSatisfy` cityFieldsAreNotFilledPredicate

    describe "Incorrect request body" $ do

      let jsonA = Null
          jsonB = String "foo"
          jsonC = Object [("foo", String "bar")]
          jsonD = Array [testData']

      let failurePredicate (Right (Object kv)) =
            not hasCaseId && acceptCode == Just (String "INCORRECT_FORMAT")

            where hasCaseId = HM.member "cardidProvider" kv
                  -- ^ Only successful constructor has this field.
                  --   Failure constructor supposed to NOT have it.

                  acceptCode = HM.lookup "acceptCode" kv

          failurePredicate _ = False

          checkFailures = do
            requestMaker <- getRequestMaker <&> \f -> f . createCallCard
            requestMaker jsonA >>= flip shouldSatisfy failurePredicate
            requestMaker jsonB >>= flip shouldSatisfy failurePredicate
            requestMaker jsonC >>= flip shouldSatisfy failurePredicate
            requestMaker jsonD >>= flip shouldSatisfy failurePredicate
            waitForBackgroundTasks

      it "Incorrect request body" $
        withTestingServer withoutTestingServer serverLock checkFailures

      describe "Incorrect requests failures are stored in the database" $ do
        it "Count of stored failures is correct" $
          withTestingServer withoutTestingServer serverLock $ do
            requestMaker  <- getRequestMaker <&> \f -> f getFailuresCount
            previousCount <- requestMaker
            unless withoutTestingServer $ previousCount `shouldBe` Right 0
            checkFailures
            requestMaker >>= flip shouldBe (previousCount <&> (+ 4))

        it "Stored correct failures data" $
          withTestingServer withoutTestingServer serverLock $ do
            checkFailures
            requestMaker <- getRequestMaker <&> \f -> f . getFailuresList . Just
            jsonListResult <- requestMaker 4

            jsonList <-
              case jsonListResult of
                   Left  e -> throwM e
                   Right x -> pure x

            let list :: Either String [Object]
                list = do
                  extractedList <-
                    case jsonList of
                         Array x -> Right $ toList x
                         _       -> Left "Root value is not an Array"

                  let -- | Accumulates in reversed order
                      f _               x@(Left  _) = x
                      f []              x@(Right _) = x
                      f (Object x : xs) (Right acc) = f xs $ Right $ x : acc
                      f _ _ = Left "Element value of an Array is not an Object"

                  f extractedList $ Right []

            fmap length list `shouldBe` Right 4
            let references = [jsonA, jsonB, jsonC, jsonD] :: [Value]
            length references `shouldBe` 4

            failuresList <-
              case list of
                   Left  msg -> fail msg
                   Right x   -> pure x

            forM_ failuresList $ \failure -> do

              HM.lookup "integrationPoint" failure
                `shouldBe` Just (String [qm|{EgCrm01}|])

              -- WARNING! Failures may be written in background threads so order
              --          of them in the database may be kinda random.
              HM.lookup "requestBody" failure `shouldSatisfy` \case
                Nothing -> False
                Just x  -> x `elem` references

    -- TODO implement and test response of the EG.CRM.01 request
    -- TODO check if it's saved to a database (CaRMa "Case" is created)
    -- TODO check condition when another EG Call Card with same VIN is coming
    --      in next 24 hours from last one just using same CaRMa "Case" for
    --      that, but also check that new CaRMa "Case" created when VIN is
    --      different.

  where
    -- | Produces @ClientEnv@ and returns requester monad.
    getRequestMaker :: IO (ClientM a -> IO (Either ServantError a))
    getRequestMaker =
      getClientEnv <&!> \clientEnv req -> runClientM req clientEnv

    -- | Waits until all background tasks is done
    waitForBackgroundTasks :: Expectation
    waitForBackgroundTasks = do
      requestMaker <- getRequestMaker <&> \f -> f getBackgroundTasksCount
      requestMaker >>= \case
        Left exception -> throw exception
        Right 0 -> pure ()
        Right _ -> threadDelay (100 * 1000) >> waitForBackgroundTasks


createCallCard          :: Value -> ClientM Value
getFailuresCount        :: ClientM Word
getFailuresList         :: Maybe Word -> ClientM Value
getBackgroundTasksCount :: ClientM Word
getCase                 :: CaseId -> ClientM Value
(      createCallCard
  :<|> (    (getFailuresCount :<|> getFailuresList)
       :<|> getBackgroundTasksCount
       :<|> getCase
       )
  ) = client (Proxy :: Proxy ServerAPI)


getClientEnv :: IO ClientEnv
getClientEnv =
  ClientEnv <$> newManager tlsManagerSettings <*> retrieveServerBaseUrl
  where
    retrieveServerBaseUrl = do
      cfg                  <- Conf.load [Conf.Required "app.cfg"]
      !(port :: Warp.Port) <- Conf.require cfg "port"
      !(host :: String)    <- Conf.lookupDefault "127.0.0.1" cfg "host"
      parseBaseUrl [qm| http://{host}:{port} |]
