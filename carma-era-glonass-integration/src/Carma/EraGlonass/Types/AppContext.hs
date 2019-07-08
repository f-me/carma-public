{-# LANGUAGE ExplicitNamespaces, DataKinds #-}

module Carma.EraGlonass.Types.AppContext
     ( type AppContext   (..)
     , type AppMode      (..)
     , type DBConnection (..)
     ) where

import           Data.Pool (type Pool)

import           Control.Concurrent.STM.TQueue (type TQueue)
import           Control.Concurrent.STM.TVar (type TVar)
import           Control.Concurrent.STM.TSem (type TSem)

import           Database.Persist.Sql (type SqlBackend)

import           Servant.Client (type ClientEnv)

import           Carma.Monad.LoggerBus.Types (type LogMessage)

import           Carma.EraGlonass.Types.EGContractId (type EGContractId)
import           Carma.EraGlonass.Types.EGIntegrationPoint
                   ( type EGIntegrationPoint (BindVehicles)
                   )


-- | Application context which holds shared data.
data AppContext
   = AppContext
   { appMode :: AppMode
       -- ^ See @AppMode@ description for details.
       --
       -- This field placed to @AppContext@ for a situation where it have to be
       -- checked inside some route.

   , loggerBus :: TQueue LogMessage
       -- ^ A bus to send log messages to.

   , dbConnection :: DBConnection
       -- ^ A @Pool@ of connections to PostgreSQL or single connection.

   , dbRequestTimeout :: Int
       -- ^ Timeout in microseconds after which database request will fail.

   , backgroundTasksCounter :: TVar Word
       -- ^ Every big operation or an operation which affects DB data supposed
       --   to increment this counter and decrement it when it finishes.
       --
       -- For tests it helps to detect when everything is done at the moment.

   , egClientEnv :: ClientEnv
       -- ^ @ClientEnv@ with bound base URL for CaRMa -> Era Glonass requests.

   , vinSynchronizerTimeout :: Int
       -- ^ VIN synchronization iteration timeout in microseconds.
       --
       -- One iteration includes requests to EG side and own database requests.

   , vinSynchronizerRetryInterval :: Int
       -- ^ An interval (in microseconds) between next VIN synchronization
       --   attempt if previous one is failed.

   , vinSynchronizerContractId :: EGContractId 'BindVehicles
       -- ^ Predefined on Era Glonass side code.
   }


-- | Application mode that indicates whether it's either production mode that
--   connects to PostgreSQL database or testing mode with SQLite in-memory
--   database.
data AppMode
   = ProductionAppMode
   | TestingAppMode
     deriving (Show, Eq)


-- | A container to bring database connection to route handlers.
data DBConnection
   -- | In-memory SQLite database cannot have @Pool@.
   = DBConnection TSem SqlBackend
   | DBConnectionPool (Pool SqlBackend)
