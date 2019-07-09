{-# LANGUAGE ExplicitNamespaces, DataKinds #-}

-- | Client requests to Era Glonass implementation.
module Carma.EraGlonass.Client
     ( bindVehicles
     , unbindVehicles
     , changeProcessingStatusRequest
     , changeRequestStatusRequest
     ) where

import           Data.Proxy

import           Servant (type (:<|>) ((:<|>)))
import           Servant.Client (type ClientM, client)

import           Carma.EraGlonass.Routes (type OutcomingAPI)
import           Carma.EraGlonass.Types.EGMayFailToParse (type EGMayFailToParse)
import           Carma.EraGlonass.Types.EGBindVehiclesRequest
                   ( type EGBindVehiclesRequest
                   , type EGBindVehiclesResponse
                   , type EGBindVehiclesMode (..)
                   )
import           Carma.EraGlonass.Types.EGChangeProcessingStatusRequest
                   ( type EGChangeProcessingStatusRequest
                   , type EGChangeProcessingStatusResponse
                   )
import           Carma.EraGlonass.Types.EGChangeRequestStatusRequest
                   ( type EGChangeRequestStatusRequest
                   )


bindVehicles
  :: EGBindVehiclesRequest 'Bind
  -> ClientM (EGMayFailToParse (EGBindVehiclesResponse 'Bind))

unbindVehicles
  :: EGBindVehiclesRequest 'Unbind
  -> ClientM (EGMayFailToParse (EGBindVehiclesResponse 'Unbind))

changeProcessingStatusRequest
  :: [EGChangeProcessingStatusRequest]
  -> ClientM (EGMayFailToParse EGChangeProcessingStatusResponse)

changeRequestStatusRequest
  :: [EGChangeRequestStatusRequest]
  -> ClientM (EGMayFailToParse EGChangeProcessingStatusResponse)

( (bindVehicles :<|> unbindVehicles)
  :<|> changeProcessingStatusRequest
  :<|> changeRequestStatusRequest
  )
  = client (Proxy :: Proxy OutcomingAPI)
