{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Function ((&))
import qualified Data.Configurator as Conf
import           Data.Pool (Pool)
import           Data.String (fromString)
import           Text.InterpolatedString.QM

import           Control.Monad.Reader (runReaderT)
import           Control.Monad.Logger (runStdoutLoggingT)
import           Control.Monad.IO.Class (liftIO)

import qualified Network.Wai.Handler.Warp as Warp
import           Database.Persist.Postgresql

import           Carma.EraGlonass.Server (serverApplicaton)
import           Carma.EraGlonass.Types (AppContext (..))
import           Carma.EraGlonass.Logger () -- ^ Logger instances
import           Carma.EraGlonass.Logger.LoggerForward (runLoggerForward)
import           Carma.EraGlonass.Persistent ()
import           Carma.Monad.LoggerBus.MonadLogger
import           Carma.Monad.LoggerBus
import           Carma.Monad.Thread
import           Carma.Monad.MVar

import           Carma.Model.Program.Persistent


main :: IO ()
main = do
  cfg <- Conf.load [Conf.Required "app.cfg"]

  !(port :: Warp.Port) <- Conf.require cfg "port"
  !(host :: String)    <- Conf.lookupDefault "127.0.0.1" cfg "host"

  !pgConf <- PostgresConf
    <$> Conf.require cfg "db.postgresql.connection-string"
    <*> Conf.require cfg "db.postgresql.pool-size"

  loggerBus' <- newEmptyMVar

  -- Running logger thread
  _ <- fork $ runStdoutLoggingT $
    writeLoggerBusEventsToMonadLogger `runReaderT` loggerBus'

  flip runReaderT loggerBus' $ do

    -- TODO Better log message about connecting to the database.
    logError [qmb|
      postgresConnStr: "{pgConnStr pgConf}"
      postgresPoolSize: {pgPoolSize pgConf}
    |]

    !(pgPool :: Pool SqlBackend) <- liftIO $
      createPostgresqlPool (pgConnStr pgConf) (pgPoolSize pgConf)
        `runLoggerForward` loggerBus'

    -- Request at start to check if it's connected (connection is lazy).
    -- TODO Check for @Just@ instead of @print@.
    testProgram <- liftIO (runSqlPool (get ford) pgPool)
    logInfo [qm| Test "Program": {testProgram} |]

    logInfo [qm| Running incoming server on http://{host}:{port}... |]

    let appContext
          = AppContext
          { loggerBus = loggerBus'
          , dbConnectionPool = pgPool
          }

    liftIO $ runIncomingServer appContext port $ fromString host


runIncomingServer :: AppContext -> Warp.Port -> Warp.HostPreference -> IO ()
runIncomingServer appContext port host
  = Warp.runSettings warpSettings
  $ serverApplicaton appContext

  where warpSettings
          = Warp.defaultSettings
          & Warp.setPort port
          & Warp.setHost host
