
module Carma.EraGlonass.Types
     ( AppContext (..)
     , AppMode (..)
     , DBConnection (..)
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
   { appMode :: AppMode
     -- ^ See @AppMode@ description for details.
     --   This field placed to @AppContext@ for a situation where it have to be
     --   checked inside some route.

   , loggerBus :: MVar LogMessage
     -- ^ A bus to send log messages to

   , dbConnection :: DBConnection
     -- ^ A @Pool@ of connections to PostgreSQL or single connection
   }


-- | Application mode that indicates if it's production mode that connects to
-- PostgreSQL database or it is testing with SQLite in-memory database.
data AppMode
   = ProductionAppMode
   | TestingAppMode
     deriving (Show, Eq)


data DBConnection
   = DBConnection SqlBackend
     -- ^ In-memory SQLite database cannot have @Pool@
   | DBConnectionPool (Pool SqlBackend)
