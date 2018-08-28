
module Carma.EraGlonass.Types
     ( AppContext (..)
     , EGCreateCallCardRequest (..)
     , EGCreateCallCardRequestGis (..)
     , EGCreateCallCardRequestVehicle (..)
     , EGCreateCallCardResponse (..)
     , EGPhoneNumber.EGPhoneNumber (EGPhoneNumber.EGPhoneNumber)
     , EGLatLon.EGLatitude, EGLatLon.toEGLatitude, EGLatLon.fromEGLatitude
     , EGLatLon.EGLongitude, EGLatLon.toEGLongitude, EGLatLon.fromEGLongitude
     , EGCallCardId.EGCallCardId (EGCallCardId.EGCallCardId)
     , EGCallerFullName.EGCallerFullName (EGCallerFullName.EGCallerFullName)
     , EGVin.EGVin (EGVin.EGVin)
     , EGAcceptCode (..)
     , module Carma.EraGlonass.Types.EGPropulsion
     ) where

import           Data.Pool (Pool)

import           Control.Concurrent.MVar (MVar)

import           Database.Persist.Sql (SqlBackend)

import           Carma.Monad.LoggerBus.Types (LogMessage)
import qualified Carma.EraGlonass.Types.EGPhoneNumber as EGPhoneNumber
import qualified Carma.EraGlonass.Types.EGLatLon as EGLatLon
import qualified Carma.EraGlonass.Types.EGCallCardId as EGCallCardId
import qualified Carma.EraGlonass.Types.EGCallerFullName as EGCallerFullName
import qualified Carma.EraGlonass.Types.EGVin as EGVin
import           Carma.EraGlonass.Types.EGPropulsion
import           Carma.EraGlonass.Types.EGAcceptCode
import           Carma.EraGlonass.Types.EGCreateCallCardRequest


data AppContext
   = AppContext
   { loggerBus :: MVar LogMessage
     -- ^ A bus to send log messages to

   , dbConnectionPool :: Pool SqlBackend
     -- ^ A @Pool@ of connections to PostgreSQL
   }
