{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

-- For Servant's stuff
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- TODO run this test from the Circle-CI container,
--      i think we need to mock database stuff for that.

module Main (main) where

import           Test.Hspec

import           Data.Proxy
import           Data.Either (isRight)
import           Text.InterpolatedString.QM
import qualified Data.Configurator as Conf
import           Data.Aeson

import qualified Network.Wai.Handler.Warp as Warp
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types.Status (Status (..))

import           Servant
import           Servant.Client

import           Carma.Utils.Operators
import           Carma.EraGlonass.Test.Types.EGCreateCallCardRequest (testData)


type ServerAPI
    =  -- Just JSON-ny representation of EG.CRM.01,
       -- without strong typing for testing purpuses,
       -- to make sure we could handle provided JSON example.
       --
       -- POST /calls/status
       --
       "calls" :> "status" :> ReqBody '[JSON] Value
                           :> Post    '[JSON] ()


main :: IO ()
main = hspec $
  describe "EG.CRM.01" egCRM01


egCRM01 :: Spec
egCRM01 =
  describe [qns| Simulating creating Call Card by Era Glonass
                 ("carma-era-glonass-integration" server must be run) |] $ do

    let !testData' = either error id testData

    it [qms| Usual successful creating EG Call Card |] $ do
      result <- getRequestMaker >>= \f -> f testData'
      result `shouldSatisfy` isRight

    it [qms| Incorrect request body |] $ do
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
    getRequestMaker :: IO (Value -> IO (Either ServantError ()))
    getRequestMaker =
      getClientEnv <&!> \clientEnv reqBody ->
        runClientM (createCallCard reqBody) clientEnv


statusCodePredicate :: Int -> Either ServantError a -> Bool
statusCodePredicate code = \case
  Left FailureResponse { responseStatus = Status { statusCode = code' } }
    | code' == code -> True
  _ -> False


createCallCard :: Value -> ClientM ()
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
