-- Client requests to Era Glonass implementation.
module Carma.EraGlonass.Client where

import           Data.Proxy

import           Servant
import           Servant.Client

import           Carma.EraGlonass.Types
import           Carma.EraGlonass.Routes


-- | TODO Support HTTP Basic Auth
crmEG02Delete
  :: EGDeleteVinRequest -> ClientM EGDeleteVinResponse

-- | TODO Support HTTP Basic Auth
crmEG02Put
  :: EGAddVinRequest -> ClientM EGAddVinResponse

-- | TODO Support HTTP Basic Auth
crmEG02Post
  :: EGCheckVinRequest -> ClientM EGCheckVinResponse


-- | TODO Support HTTP Basic Auth
crmEG03Post
  :: EGUpdateCallCardStatusRequest -> ClientM EGUpdateCallCardStatusResponse


(
  (    crmEG02Delete
  :<|> crmEG02Put
  :<|> crmEG02Post
  )

  :<|> crmEG03Post

  ) = client (Proxy :: Proxy OutcomingAPI)
