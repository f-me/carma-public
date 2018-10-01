{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Carma.EraGlonass.App
     ( AppConfig (..)
     , app
     ) where

import           Data.Function ((&))
import qualified Data.Configurator as Conf
import           Data.String (fromString)
import           Text.InterpolatedString.QM

import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Logger (runStdoutLoggingT)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Concurrent.MVar (tryReadMVar)
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM.TSem

import           System.Posix.Signals
                   ( installHandler
                   , Handler (Catch)
                   , sigINT
                   , sigTERM
                   )

import qualified Network.Wai.Handler.Warp as Warp
import           Database.Persist.Postgresql (PostgresConf (PostgresConf))

import           Carma.Monad.LoggerBus.Types (LogMessage)
import           Carma.Monad.LoggerBus.MonadLogger
import           Carma.Monad.LoggerBus
import           Carma.Monad.Thread
import           Carma.Monad.Delay
import           Carma.Monad.STM
import           Carma.EraGlonass.Types
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Server (serverApplicaton)


-- | Application config data to provide to particular implementation.
data AppConfig
   = AppConfig
   { pgConf :: PostgresConf
   , dbRequestTimeout :: Float
   }


-- | Application starter which abstract from specific database.
--
-- It takes a monad that wraps server runner and provides specific database
-- connection to the server. Server runner constructs @AppContext@ with provided
-- database connection and runs a server.
app
  :: (MonadIO m, MonadBaseControl IO m)
  => AppMode
  -> ( AppConfig
       -> (DBConnection -> ReaderT (TQueue LogMessage) m ())
       -> ReaderT (TQueue LogMessage) m ()
     ) -- ^ Database connection creator that wraps server runner
  -> m ()
app appMode' dbConnectionCreator = do
  cfg <- liftIO $ Conf.load [Conf.Required "app.cfg"]

  !(port :: Warp.Port) <- liftIO $ Conf.require cfg "port"
  !(host :: String)    <- liftIO $ Conf.lookupDefault "127.0.0.1" cfg "host"

  !pgConf' <- liftIO $ PostgresConf
    <$> Conf.require cfg "db.postgresql.connection-string"
    <*> Conf.require cfg "db.postgresql.pool-size"

  !(dbRequestTimeout' :: Float) <-
    liftIO $ Conf.require cfg "db.postgresql.request-timeout"

  loggerBus' <- atomically newTQueue

  -- Running logger thread
  (_, loggerThreadWaitBus) <-
    forkWithWaitBus $ runStdoutLoggingT $
      writeLoggerBusEventsToMonadLogger `runReaderT` loggerBus'

  backgroundTasksCounter' <- atomically $ newTVar 0

  (serverThreadId, serverThreadSem) <- flip runReaderT loggerBus' $ do

    let runServer dbConnection' = do

          let appContext
                = AppContext
                { appMode = appMode'
                , loggerBus = loggerBus'
                , dbConnection = dbConnection'
                , dbRequestTimeout = round $ dbRequestTimeout' * 1000 * 1000
                , backgroundTasksCounter = backgroundTasksCounter'
                }

          logInfo [qm| Running incoming server on http://{host}:{port}... |]
          liftIO $ runIncomingServer appContext port $ fromString host

    when (appMode' == TestingAppMode) $
      logWarn "Starting testing server with in-memory SQLite database..."

    forkWithSem $ let

      x = AppConfig
        { pgConf           = pgConf'
        , dbRequestTimeout = dbRequestTimeout'
        }

      in dbConnectionCreator x runServer

  -- Trapping termination of the application
  liftIO $ forM_ [sigINT, sigTERM] $ \sig ->
    let terminateHook = killThread serverThreadId
     in installHandler sig (Catch terminateHook) Nothing

  -- Wait for server thread
  atomically $ waitTSem serverThreadSem

  let waitForLogger =
        atomically (tryPeekTQueue loggerBus') >>= \case
          Nothing -> pure () -- We're done, successfully exiting
          Just _ -> -- Logger still have something to handle
            tryReadMVar loggerThreadWaitBus >>= \case
              Nothing ->
                -- Waiting for 100 milliseconds before next iteration
                -- to avoid high CPU usage.
                delay (100 * 1000) >> waitForLogger
              Just _ ->
                -- Something went wrong
                fail [qns| Logger thread is probably failed
                           before it finished handling all log messages! |]

  -- Making sure that logger handled all the messages from logger bus
  liftIO waitForLogger


runIncomingServer :: AppContext -> Warp.Port -> Warp.HostPreference -> IO ()
runIncomingServer appContext port host
  = Warp.runSettings warpSettings
  $ serverApplicaton appContext

  where warpSettings
          = Warp.defaultSettings
          & Warp.setPort port
          & Warp.setHost host
