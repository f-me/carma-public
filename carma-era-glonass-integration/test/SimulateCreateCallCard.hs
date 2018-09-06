{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

-- For Servant's stuff
{-# LANGUAGE DataKinds, TypeOperators #-}

module Main (main) where

import           Test.Hspec

import           Data.Proxy
import           Data.Either (isRight)
import           Text.InterpolatedString.QM
import qualified Data.Configurator as Conf
import           Data.Aeson
import qualified Data.HashMap.Lazy as HM
import           Data.Foldable (toList)

import           Control.Monad
import           Control.Monad.Catch
import           Control.Concurrent.MVar

import           System.Environment

import qualified Network.Wai.Handler.Warp as Warp
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types.Status (Status (..))

import           Servant
import           Servant.Client

import           Carma.Utils.Operators
import           Carma.EraGlonass.Test.Helpers
import           Carma.EraGlonass.Test.Types.EGCreateCallCardRequest (testData)
import           Carma.EraGlonass.Model.CaseEraGlonassFailure.Types


type ServerAPI
    =  -- Just JSON-ny representation of EG.CRM.01,
       -- without strong typing for testing purpuses,
       -- to make sure we could handle provided JSON example.
       --
       -- POST /calls/status
       --
       "calls" :> "status" :> ReqBody '[JSON] Value
                           :> Post    '[JSON] Value

  :<|> "debug"
       :> "failures"
       :> (
               -- GET /debug/failures/count.json
               "count.json" :> Get '[JSON] Word

          :<|> -- GET /debug/failures/list.json?limit=10
               "list.json" :> QueryParam "limit" Word :> Get '[JSON] Value
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
        result `shouldSatisfy` isRight

    describe "Incorrect request body" $ do

      let jsonA = Null
          jsonB = String "foo"
          jsonC = Object [("foo", String "bar")]
          jsonD = Array [testData']

      let checkFailures = do
            requestMaker <- getRequestMaker <&> \f x -> f $ createCallCard x
            requestMaker jsonA >>= flip shouldSatisfy (statusCodePredicate 400)
            requestMaker jsonB >>= flip shouldSatisfy (statusCodePredicate 400)
            requestMaker jsonC >>= flip shouldSatisfy (statusCodePredicate 400)
            requestMaker jsonD >>= flip shouldSatisfy (statusCodePredicate 400)

      it "Incorrect request body" $
        withTestingServer withoutTestingServer serverLock checkFailures

      describe "Incorrect requests failures are stored in the database" $ do
        it "Count of stored failures is correct" $
          withTestingServer withoutTestingServer serverLock $ do
            checkFailures
            requestMaker <- getRequestMaker <&> \f -> f getFailuresCount
            requestMaker >>= flip shouldBe (Right 4)

        it "Stored correct failures data" $
          withTestingServer withoutTestingServer serverLock $ do
            checkFailures
            requestMaker <-
              getRequestMaker <&> \f x -> f $ getFailuresList $ Just x
            jsonListResult <- requestMaker 10

            jsonList <-
              case jsonListResult of
                   Left err -> throwM err
                   Right x  -> pure x

            let list :: Either String [Object]
                list = do
                  extractedList <-
                    case jsonList of
                      Array x -> Right $ toList x
                      _       -> Left "Root value is not an Array"

                  let f _               x@(Left  _) = x
                      f []              x@(Right _) = x
                      f (Object x : xs) (Right acc) = f xs $ Right $ x : acc
                      f _ _ = Left "Element value of an Array is not an Object"

                  f extractedList (Right [])

            fmap length list `shouldBe` Right 4

            zippedList <-
              case list of
                   Left msg -> fail msg
                   Right x  -> pure $ zip [jsonA, jsonB, jsonC, jsonD] x

            length zippedList `shouldBe` 4

            forM_ zippedList $ \(reference, failure) -> do

              HM.lookup "integrationPoint" failure
                `shouldBe` Just (String [qm|{EgCrm01}|])

              HM.lookup "requestBody" failure `shouldBe` Just reference

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


statusCodePredicate :: Int -> Either ServantError a -> Bool
statusCodePredicate code = \case
  Left FailureResponse { responseStatus = Status { statusCode = code' } }
    | code' == code -> True
  _ -> False


createCallCard   :: Value -> ClientM Value
getFailuresCount :: ClientM Word
getFailuresList  :: Maybe Word -> ClientM Value
(createCallCard :<|> (getFailuresCount :<|> getFailuresList))
  = client (Proxy :: Proxy ServerAPI)


getClientEnv :: IO ClientEnv
getClientEnv =
  ClientEnv <$> newManager tlsManagerSettings <*> retrieveServerBaseUrl
  where
    retrieveServerBaseUrl = do
      cfg                  <- Conf.load [Conf.Required "app.cfg"]
      !(port :: Warp.Port) <- Conf.require cfg "port"
      !(host :: String)    <- Conf.lookupDefault "127.0.0.1" cfg "host"
      parseBaseUrl [qm| http://{host}:{port} |]
