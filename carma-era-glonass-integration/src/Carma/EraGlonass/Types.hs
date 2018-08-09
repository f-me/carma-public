
module Carma.EraGlonass.Types
     ( AppContext (..)
     , EGCreateCallCardRequest (..)
     , EGCreateCallCardRequestGis (..)
     , EGCreateCallCardRequestVehicle (..)
     , EGPhoneNumber.EGPhoneNumber (EGPhoneNumber.EGPhoneNumber)
     , EGLatLon.EGLatitude, EGLatLon.toEGLatitude, EGLatLon.fromEGLatitude
     , EGLatLon.EGLongitude, EGLatLon.toEGLongitude, EGLatLon.fromEGLongitude
     , EGCallCardId.EGCallCardId (EGCallCardId.EGCallCardId)
     , EGCallerFullName.EGCallerFullName (EGCallerFullName.EGCallerFullName)
     , EGVin.EGVin (EGVin.EGVin)
     , module Carma.EraGlonass.Types.EGPropulsion
     ) where

import           Control.Concurrent.MVar (MVar)

import           Carma.Monad.LoggerBus.Types (LogMessage)
import qualified Carma.EraGlonass.Types.EGPhoneNumber as EGPhoneNumber
import qualified Carma.EraGlonass.Types.EGLatLon as EGLatLon
import qualified Carma.EraGlonass.Types.EGCallCardId as EGCallCardId
import qualified Carma.EraGlonass.Types.EGCallerFullName as EGCallerFullName
import qualified Carma.EraGlonass.Types.EGVin as EGVin
import           Carma.EraGlonass.Types.EGPropulsion
import           Carma.EraGlonass.Types.EGCreateCallCardRequest


data AppContext
   = AppContext
   { -- A bus to send log messages to
     loggerBus :: MVar LogMessage
   }
