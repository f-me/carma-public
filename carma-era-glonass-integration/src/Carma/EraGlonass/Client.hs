-- Client requests to Era Glonass implementation.
module Carma.EraGlonass.Client where

import           Data.Proxy

import           Servant
import           Servant.Client

import           Carma.EraGlonass.Types
import           Carma.EraGlonass.Routes


crmEG02Delete
  :: ClientM ()

crmEG02Put
  :: ClientM ()

crmEG02Post
  :: EGCheckVinRequest -> ClientM EGCheckVinResponse


crmEG03Post
  :: EGUpdateCallCardStatusRequest -> ClientM EGUpdateCallCardStatusResponse


(
  (    crmEG02Delete
  :<|> crmEG02Put
  :<|> crmEG02Post
  )

  :<|> crmEG03Post

  ) = client (Proxy :: Proxy OutcomingAPI)
