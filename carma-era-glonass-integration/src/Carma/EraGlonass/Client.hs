-- Client requests to Era Glonass implementation.
module Carma.EraGlonass.Client where

import           Data.Proxy

import           Servant
import           Servant.Client

import           Carma.EraGlonass.Routes


crmEG02Delete
  :: ClientM ()

crmEG02Put
  :: ClientM ()

crmEG02Post
  :: ClientM ()


crmEG03Put
  :: ClientM ()

crmEG03Post
  :: ClientM ()


(
  (    crmEG02Delete
  :<|> crmEG02Put
  :<|> crmEG02Post
  )

  :<|>

  (    crmEG03Put
  :<|> crmEG03Post
  )

  ) = client (Proxy :: Proxy OutcomingAPI)
