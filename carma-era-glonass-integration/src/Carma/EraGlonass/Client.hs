{-# LANGUAGE ExplicitNamespaces #-}

-- | Client requests to Era Glonass implementation.
module Carma.EraGlonass.Client
     ( bindVehicles
     , changeProcessingStatusRequest
     , changeRequestStatusRequest
     ) where

import           Data.Proxy

import           Servant (type (:<|>) ((:<|>)))
import           Servant.Client (type ClientM, client)

import           Carma.EraGlonass.Routes (type OutcomingAPI)
import           Carma.EraGlonass.Types.EGBindVehiclesRequest
                   ( type EGBindVehiclesRequest
                   , type EGBindVehiclesResponse
                   )
import           Carma.EraGlonass.Types.EGChangeProcessingStatusRequest
                   ( type EGChangeProcessingStatusRequest
                   , type EGChangeProcessingStatusResponse
                   )
import           Carma.EraGlonass.Types.EGChangeRequestStatusRequest
                   ( type EGChangeRequestStatusRequest
                   )


bindVehicles
  :: EGBindVehiclesRequest
  -> ClientM EGBindVehiclesResponse

changeProcessingStatusRequest
  :: [EGChangeProcessingStatusRequest]
  -> ClientM EGChangeProcessingStatusResponse

changeRequestStatusRequest
  :: [EGChangeRequestStatusRequest]
  -> ClientM EGChangeProcessingStatusResponse

( bindVehicles
  :<|> changeProcessingStatusRequest
  :<|> changeRequestStatusRequest
  )
  = client (Proxy :: Proxy OutcomingAPI)
