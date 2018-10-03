{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns, LambdaCase, ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

-- For Servant's stuff
{-# LANGUAGE DataKinds, TypeOperators #-}

module Main (main) where

import           Test.Hspec

import           Data.Proxy
import           Data.Maybe (isJust, fromJust)
import qualified Data.Vector as V
import           Data.Text (Text)
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

import           Database.Persist.Sql (fromSqlKey, toSqlKey)

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
      withTestingServerWrap $ do
        result <- getRequestMaker >>= \f -> f $ createCallCard testData'
        result `shouldSatisfy` \case
          Right (Object kv) -> let

            hasCaseId = HM.member "cardidProvider" kv
            -- ^ Only successful constructor have this field

            in HM.lookup "acceptCode" kv == Just (String "OK") && hasCaseId

          _ -> False

    it "Returned Case does exist" $
      withTestingServerWrap $ do
        result <-
          (getRequestMaker >>= \f -> f $ createCallCard testData')
            <&> fmap (\case Object x -> HM.lookup "cardidProvider" x
                            _        -> Nothing)
            <&> fmap (\case Just (String x) -> Just x
                            _               -> Nothing)

        result `shouldSatisfy` \case
          Right (Just _) -> True
          _ -> False

        getCaseRequest <- getRequestMaker <&> \f -> f . getCase

        let caseId :: CaseId
            caseId =
              result
              & either (\e -> error [qm| unexpected exception: {e} |]) id
              & fromJust
              & toSqlKey . (\x -> read [qm|{x}|])

        getCaseRequest caseId >>= \caseData ->
          caseData `shouldSatisfy` \case
            Right _ -> True
            Left  _ -> False

        let fakeCaseId = toSqlKey $ fromSqlKey caseId + 100 :: CaseId

        -- Case with fake ID must not exist
        getCaseRequest fakeCaseId >>= \caseData ->
          caseData `shouldSatisfy` \case
            Right _ -> False
            Left  _ -> True

    describe "Dictionary city fields of a case are filled by gis data" $ do

      let obtainCase :: MonadThrow m => Either ServantError Value -> m CaseId
          obtainCase = \case
            Right (Object kv) ->
              case HM.lookup k kv of
                   Just (String textCaseId) ->
                     pure $ toSqlKey $ read [qm| {textCaseId} |]
                   Just x -> fail [qm| "{k}" key is not a "String": {x} |]
                   Nothing -> fail [qm| "{k}" key not found! |]

            Right x -> fail [qm| Root is not an "Object": {x} |]
            Left err -> throwM err

            where k = "cardidProvider"

      let cityFieldsAreFilledPredicate :: Either a Value -> Bool
          cityFieldsAreFilledPredicate = \case
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

      let cityFieldsAreNotFilledPredicate :: Either a Value -> Bool
          cityFieldsAreNotFilledPredicate = \case
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

      it "Found city by its label" $ do
        randomVin <- genRandomVin

        withTestingServerWrap $ do
          result <-
            getRequestMaker >>= \f -> f $ createCallCard $
              either error id $ testData >>= setRandomVin randomVin

          caseId <- obtainCase result
          caseData <- getRequestMaker >>= \f -> f $ getCase caseId
          caseData `shouldSatisfy` cityFieldsAreFilledPredicate
          caseData `shouldNotSatisfy` cityFieldsAreNotFilledPredicate

      it "City with such label not exists" $ do
        randomVin <- genRandomVin

        withTestingServerWrap $ do
          result <- getRequestMaker >>= \f -> f $ createCallCard $ let

            -- | Modifier of "gis.settlementName" key to prevent city
            -- dictionary __@Case@__'s field from detecting.
            setUnknownCity :: Value -> Either String Value
            setUnknownCity = modifyObjectProp "gis" $ \case

              Array (V.toList -> (x@(Object _) : xs)) -> do
                newGis <- fGisItem x
                Right $ Array $ V.fromList (newGis : xs)

              x -> Left [qm| Not an "Array" of "Object"s: {x} |]

            fGisItem :: Value -> Either String Value
            fGisItem = modifyObjectProp "settlementName" $ \case
              String _ -> Right $ String "unknown city label plug"
              x        -> Left [qm| Not a "String": {x} |]

            in either error id $
                 testData >>= setUnknownCity >>= setRandomVin randomVin

          caseId <- obtainCase result
          caseData <- getRequestMaker >>= \f -> f $ getCase caseId
          caseData `shouldNotSatisfy` cityFieldsAreFilledPredicate
          caseData `shouldSatisfy` cityFieldsAreNotFilledPredicate

    describe "Call Card merged with already existing Case" $ do

      let obtainCaseId :: MonadThrow m => Either ServantError Value -> m Text

          obtainCaseId (Right
                       (Object
                       (HM.lookup "cardidProvider" -> Just (String x)))) =
                         pure x

          obtainCaseId (Right x) = fail [qm| Incorrect response: {x} |]
          obtainCaseId (Left exception) = throwM exception

      it "Merged when VIN is the same (in next 24 hours)" $
        withTestingServerWrap $ do
          createCC <- getRequestMaker <&> \f -> f . createCallCard
          caseIdFirst <- createCC testData' >>= obtainCaseId
          caseIdSecond <- createCC testData' >>= obtainCaseId
          caseIdFirst `shouldBe` caseIdSecond

      it "Not merged when VIN is different" $ do
        randomVin <- genRandomVin

        withTestingServerWrap $ do
          createCC <- getRequestMaker <&> \f -> f . createCallCard
          caseIdFirst <- createCC testData' >>= obtainCaseId

          caseIdSecond <-
            createCC (either error id $ testData >>= setRandomVin randomVin)
              >>= obtainCaseId

          caseIdFirst `shouldNotBe` caseIdSecond

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
        withTestingServerWrap checkFailures

      describe "Incorrect requests failures are stored in the database" $ do
        it "Count of stored failures is correct" $
          withTestingServerWrap $ do
            requestMaker  <- getRequestMaker <&> \f -> f getFailuresCount
            previousCount <- requestMaker
            unless withoutTestingServer $ previousCount `shouldBe` Right 0
            checkFailures
            requestMaker >>= flip shouldBe (previousCount <&> (+ 4))

        it "Stored correct failures data" $
          withTestingServerWrap $ do
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

  where
    -- | Shorthand for testing server wrapper.
    withTestingServerWrap = withTestingServer withoutTestingServer serverLock

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

    -- | Generates random VIN.
    --
    -- Useful to avoid merging with already exiting @Case@.
    genRandomVin :: IO String
    genRandomVin =
      getRandomRs ('A', 'Z')
        <&> take 17
        <&> map (\case 'I' -> '0'
                       'O' -> '1'
                       'Q' -> '2'
                       x -> x)

    -- | "vehicle.vin" key modifier to prevent from merging with already
    -- existing __@Case@__.
    setRandomVin :: String -> Value -> Either String Value
    setRandomVin randomVin
      = modifyObjectProp "vehicle"
      $ modifyObjectProp "vin"
      $ \case String _ -> Right $ String [qm| {randomVin} |]
              x        -> Left [qm| Not a "String": {x} |]


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
