{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

-- For Servant's stuff
{-# LANGUAGE DataKinds, TypeOperators #-}

module Main (main) where

import           Test.Hspec

import           Data.Proxy
import           Data.Either (isRight)
import           Text.InterpolatedString.QM
import qualified Data.Configurator as Conf
import           Data.Aeson
import qualified Data.Attoparsec.Text as ParsecText
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.HashMap.Lazy as HM
import           Data.Foldable (toList)

import           Control.Monad
import           Control.Monad.Catch
import           Control.Concurrent

import           System.Process
import           System.IO

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

  hspec $
    describe "EG.CRM.01" $ egCRM01 serverLock


-- | Wrapper that starts testing HTTP server in background.
--
-- It terminates it (testing HTTP server) when wrapped monad is done.
-- Testing server using new clean SQLite in-memory database.
withTestingServer :: MVar () -> Expectation -> Expectation
withTestingServer locker runTest = do
  () <- takeMVar locker
  (Nothing, Just hOut, Just hErr, hProc) <- createProcess serverCmd
  (outLogMVar :: MVar T.Text) <- newEmptyMVar
  (errLogMVar :: MVar T.Text) <- newEmptyMVar

  _ <-
    let
      f :: T.Text -> IO ()
      f accumulator = do
        isReadable <- (&&) <$> (not <$> hIsEOF hErr) <*> hIsReadable hErr
        if not isReadable
           then putMVar errLogMVar accumulator
           else T.hGetLine hErr >>= \x -> f [qm| {accumulator}  {x}\n |]
    in
      forkIO $ f mempty

  let readLog :: T.Text -> IO ()
      readLog outLog = do
        getProcessExitCode hProc >>= \case
          Nothing -> pure ()
          Just x  -> do errLog <- takeMVar errLogMVar
                        fail [qmb| Testing server is failed with exit code: {x}!
                                   Error log:\n{errLog} |]

        isReadable <- (&&) <$> (not <$> hIsEOF hOut) <*> hIsReadable hOut
        if not isReadable
           then readLog outLog
                -- ^ Recursive repeat to catch unexpected termination
           else T.hGetLine hOut
                  <&> (\x -> (x, ParsecText.parseOnly substr x))
                  >>= \case (l, Left  _) -> readLog [qm| {outLog}  {l}\n |]
                            (l, Right _) ->
                              putMVar outLogMVar [qm| {outLog}  {l}\n |]
                              -- ^ Server is ready, we're done here

  let testFailureHandler :: ServantError -> IO ()
      testFailureHandler exception = do
        _ <- terminateProcess hProc >> waitForProcess hProc
        errLog <- takeMVar errLogMVar
        outLog <- takeMVar outLogMVar

        fail [qmb|
          Test is failed because of test server response:
          \  {exception}
          Testing server stderr:
            {if errLog == mempty then "  <log is empty>\n" else errLog}\
          Testing server stdout:
            {if outLog == mempty then "  <log is empty>\n" else outLog}
        |]

  finally (readLog mempty >> (runTest `catch` testFailureHandler)) $ do
    _ <- terminateProcess hProc >> waitForProcess hProc
    putMVar locker ()

  where
    substr = findSubstring "Running incoming server on"

    serverCmd =
      (proc "stack" ["exec", "carma-era-glonass-integration-test-server"])
        { std_in  = Inherit
        , std_out = CreatePipe
        , std_err = CreatePipe
        }


egCRM01 :: MVar () -> Spec
egCRM01 serverLock =
  describe "Simulating creating Call Card by Era Glonass" $ do

    let !testData' = either error id testData

    it "Usual successful creating EG Call Card" $
      withTestingServer serverLock $ do
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
        withTestingServer serverLock checkFailures

      describe "Incorrect requests failures are stored in the database" $ do
        it "Count of stored failures is correct" $
          withTestingServer serverLock $ do
            checkFailures
            requestMaker <- getRequestMaker <&> \f -> f getFailuresCount
            requestMaker >>= flip shouldBe (Right 4)

        it "Stored correct failures data" $
          withTestingServer serverLock $ do
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
    getRequestMaker :: forall a. IO (ClientM a -> IO (Either ServantError a))
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
