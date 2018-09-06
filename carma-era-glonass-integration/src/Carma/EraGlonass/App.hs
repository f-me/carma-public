{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Carma.EraGlonass.App
     ( app
     ) where

import           Data.Function ((&))
import qualified Data.Configurator as Conf
import           Data.String (fromString)
import           Text.InterpolatedString.QM

import           Control.Monad (when)
import           Control.Monad.Reader
import           Control.Monad.Logger (runStdoutLoggingT)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Trans.Control (MonadBaseControl)

import qualified Network.Wai.Handler.Warp as Warp
import           Database.Persist.Postgresql (PostgresConf (PostgresConf))

import           Carma.Monad.LoggerBus.Types (LogMessage)
import           Carma.Monad.LoggerBus.MonadLogger
import           Carma.Monad.LoggerBus
import           Carma.Monad.Thread
import           Carma.Monad.MVar
import           Carma.EraGlonass.Types
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Server (serverApplicaton)


-- | Application starter which abstract from specific database.
--
-- It takes a monad that wraps server runner and provides specific database
-- connection to the server. Server runner constructs @AppContext@ with provided
-- database connection and runs a server.
app
  :: (MonadIO m, MonadBaseControl IO m)
  => AppMode
  -> ( PostgresConf
       -> (DBConnection -> ReaderT (MVar LogMessage) m ())
       -> ReaderT (MVar LogMessage) m ()
     ) -- ^ Database connection creator that wraps server runner
  -> m ()
app appMode' dbConnectionCreator = do
  cfg <- liftIO $ Conf.load [Conf.Required "app.cfg"]

  !(port :: Warp.Port) <- liftIO $ Conf.require cfg "port"
  !(host :: String)    <- liftIO $ Conf.lookupDefault "127.0.0.1" cfg "host"

  !pgConf <- liftIO $ PostgresConf
    <$> Conf.require cfg "db.postgresql.connection-string"
    <*> Conf.require cfg "db.postgresql.pool-size"

  loggerBus' <- newEmptyMVar

  -- Running logger thread
  _ <- fork $ runStdoutLoggingT $
    writeLoggerBusEventsToMonadLogger `runReaderT` loggerBus'

  flip runReaderT loggerBus' $ do

    let runServer dbConnection' = do

          let appContext
                = AppContext
                { appMode = appMode'
                , loggerBus = loggerBus'
                , dbConnection = dbConnection'
                }

          logInfo [qm| Running incoming server on http://{host}:{port}... |]
          liftIO $ runIncomingServer appContext port $ fromString host

    when (appMode' == TestingAppMode) $
      logWarn "Starting testing server with in-memory SQLite database..."

    dbConnectionCreator pgConf runServer


runIncomingServer :: AppContext -> Warp.Port -> Warp.HostPreference -> IO ()
runIncomingServer appContext port host
  = Warp.runSettings warpSettings
  $ serverApplicaton appContext

  where warpSettings
          = Warp.defaultSettings
          & Warp.setPort port
          & Warp.setHost host
