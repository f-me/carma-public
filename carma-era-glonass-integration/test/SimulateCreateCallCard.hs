{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

-- For Servant's stuff
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import           Test.Hspec

import           Data.Proxy
import           Data.Either (isRight)
import           Text.InterpolatedString.QM
import qualified Data.Configurator as Conf
import           Data.Aeson
import qualified Data.Attoparsec.Text as ParsecText
import           Data.String (IsString (fromString))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Control.Monad
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


type ServerAPI
    =  -- Just JSON-ny representation of EG.CRM.01,
       -- without strong typing for testing purpuses,
       -- to make sure we could handle provided JSON example.
       --
       -- POST /calls/status
       --
       "calls" :> "status" :> ReqBody '[JSON] Value
                           :> Post    '[JSON] Value


main :: IO ()
main = hspec $
  describe "EG.CRM.01" egCRM01


withTestingServer :: Expectation -> Expectation
withTestingServer runTest = do
  (Nothing, Just hOut, Just hErr, hProc) <- createProcess serverCmd
  errLogMVar <- newEmptyMVar

  _ <-
    let
      f :: Text.Text -> IO ()
      f accumulator = do
        isReadable <- (&&) <$> (not <$> hIsEOF hErr) <*> hIsReadable hErr
        if not isReadable
           then putMVar errLogMVar accumulator
           else Text.hGetLine hErr >>= \x -> f [qm| {accumulator}  {x}\n |]
    in
      forkIO $ f mempty

  let readLog :: IO ()
      readLog = do
        getProcessExitCode hProc >>= \case
          Nothing -> pure ()
          Just x  -> do errLog <- takeMVar errLogMVar
                        fail [qmb| Testing server is failed with exit code: {x}!
                                   Error log:\n{errLog} |]

        isReadable <- (&&) <$> (not <$> hIsEOF hOut) <*> hIsReadable hOut
        if not isReadable
           then readLog -- ^ Recursive repeat to catch unexpected termination
           else ParsecText.parseOnly substr . fromString <$> hGetLine hOut
                  >>= \case Left  _ -> readLog -- ^ Recursive repeat
                            Right _ -> pure () -- ^ Server is ready

  readLog
  runTest
  void $ terminateProcess hProc >> waitForProcess hProc

  where
    substr = findSubstring "Running incoming server on"

    serverCmd =
      (proc "stack" ["exec", "carma-era-glonass-integration-test-server"])
        { std_in  = Inherit
        , std_out = CreatePipe
        , std_err = CreatePipe
        }


egCRM01 :: Spec
egCRM01 =
  describe [qns| Simulating creating Call Card by Era Glonass |] $ do

    let !testData' = either error id testData

    it [qms| Usual successful creating EG Call Card |] $
      withTestingServer $ do
        result <- getRequestMaker >>= \f -> f testData'
        result `shouldSatisfy` isRight

    it [qms| Incorrect request body |] $
      withTestingServer $ do
        requestMaker <- getRequestMaker
        requestMaker Null >>= flip shouldSatisfy (statusCodePredicate 400)
        requestMaker (String "foo")
          >>= flip shouldSatisfy (statusCodePredicate 400)
        requestMaker (Object [("foo", String "bar")])
          >>= flip shouldSatisfy (statusCodePredicate 400)
        requestMaker (Array [testData'])
          >>= flip shouldSatisfy (statusCodePredicate 400)

    -- TODO implement and test response of the EG.CRM.01 request
    -- TODO check if it's saved to a database (CaRMa "Case" is created)
    -- TODO check condition when another EG Call Card with same VIN is coming
    --      in next 24 hours from last one just using same CaRMa "Case" for
    --      that, but also check that new CaRMa "Case" created when VIN is
    --      different.

  where
    getRequestMaker :: IO (Value -> IO (Either ServantError Value))
    getRequestMaker =
      getClientEnv <&!> \clientEnv reqBody ->
        runClientM (createCallCard reqBody) clientEnv


statusCodePredicate :: Int -> Either ServantError a -> Bool
statusCodePredicate code = \case
  Left FailureResponse { responseStatus = Status { statusCode = code' } }
    | code' == code -> True
  _ -> False


createCallCard :: Value -> ClientM Value
createCallCard = client (Proxy :: Proxy ServerAPI)


getClientEnv :: IO ClientEnv
getClientEnv =
  ClientEnv <$> newManager tlsManagerSettings <*> retrieveServerBaseUrl
  where
    retrieveServerBaseUrl = do
      cfg                  <- Conf.load [Conf.Required "app.cfg"]
      !(port :: Warp.Port) <- Conf.require cfg "port"
      !(host :: String)    <- Conf.lookupDefault "127.0.0.1" cfg "host"
      parseBaseUrl [qm| http://{host}:{port} |]
