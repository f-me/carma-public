
module Carma.EraGlonass.Types
     ( AppContext (..)
     , AppMode (..)
     , DBConnection (..)
     , EGPhoneNumber.EGPhoneNumber (..)
     , EGLatLon.EGLatitude, EGLatLon.toEGLatitude, EGLatLon.fromEGLatitude
     , EGLatLon.EGLongitude, EGLatLon.toEGLongitude, EGLatLon.fromEGLongitude
     , EGCallCardId.EGCallCardId (..)
     , EGCallerFullName.EGCallerFullName (..)
     , EGVin.EGVin (..)
     , EGAcceptCode (..)
     , module Carma.EraGlonass.Types.EGPropulsion
     , module Carma.EraGlonass.Types.RequestId

     , EGCreateCallCardRequest (..)
     , EGCreateCallCardRequestGis (..)
     , EGCreateCallCardRequestVehicle (..)
     , EGCreateCallCardResponse (..)

     , EGUpdateCallCardStatusRequest (..)
     , EGUpdateCallCardStatusRequestRequests (..)
     , EGUpdateCallCardStatusRequestStatus (..)
     , EGUpdateCallCardStatusResponse (..)
     , EGUpdateCallCardStatusResponseResponses (..)

     , EGAddVinRequest (..)
     , EGAddVinRequestRequests (..)
     , EGAddVinResponse (..)
     , EGAddVinResponseResponses (..)

     , EGCheckVinRequest (..)
     , EGCheckVinRequestRequests (..)
     , EGCheckVinResponse (..)
     , EGCheckVinResponseResponses (..)
     , EGCheckVinResponseVinProviders (..)

     , EGDeleteVinRequest (..)
     , EGDeleteVinRequestRequests (..)
     , EGDeleteVinResponse (..)
     , EGDeleteVinResponseResponses (..)
     ) where

import           Data.Pool (Pool)

import           Control.Concurrent.STM.TQueue (TQueue)
import           Control.Concurrent.STM.TVar (TVar)
import           Control.Concurrent.STM.TSem (TSem)

import           Database.Persist.Sql (SqlBackend)

import           Carma.Monad.LoggerBus.Types (LogMessage)
import qualified Carma.EraGlonass.Types.EGPhoneNumber as EGPhoneNumber
import qualified Carma.EraGlonass.Types.EGLatLon as EGLatLon
import qualified Carma.EraGlonass.Types.EGCallCardId as EGCallCardId
import qualified Carma.EraGlonass.Types.EGCallerFullName as EGCallerFullName
import qualified Carma.EraGlonass.Types.EGVin as EGVin
import           Carma.EraGlonass.Types.EGPropulsion
import           Carma.EraGlonass.Types.EGAcceptCode
import           Carma.EraGlonass.Types.RequestId
import           Carma.EraGlonass.Types.EGCreateCallCardRequest
import           Carma.EraGlonass.Types.EGUpdateCallCardStatusRequest
import           Carma.EraGlonass.Types.EGAddVinRequest
import           Carma.EraGlonass.Types.EGCheckVinRequest
import           Carma.EraGlonass.Types.EGDeleteVinRequest


-- | Application context which holds shared data
data AppContext
   = AppContext
   { appMode :: AppMode
     -- ^ See @AppMode@ description for details.
     --   This field placed to @AppContext@ for a situation where it have to be
     --   checked inside some route.

   , loggerBus :: TQueue LogMessage
     -- ^ A bus to send log messages to

   , dbConnection :: DBConnection
     -- ^ A @Pool@ of connections to PostgreSQL or single connection

   , dbRequestTimeout :: Int
     -- ^ Timeout in microseconds after which database request will fail

   , backgroundTasksCounter :: TVar Word
     -- ^ Every big operation or an operation which affects DB data supposed to
     -- increment this counter and decrement it when it finishes. For tests it
     -- helps to detect when everything is done at the moment.
   }


-- | Application mode that indicates if it's production mode that connects to
--   PostgreSQL database or it is testing with SQLite in-memory database.
data AppMode
   = ProductionAppMode
   | TestingAppMode
     deriving (Show, Eq)


-- | A container to bring database connection to route handlers
data DBConnection
   = DBConnection TSem SqlBackend
     -- ^ In-memory SQLite database cannot have @Pool@
   | DBConnectionPool (Pool SqlBackend)
