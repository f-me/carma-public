{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Function ((&))
import qualified Data.Configurator as Conf
import           Data.String (fromString)
import           Text.InterpolatedString.QM

import           Control.Monad.Reader (runReaderT)
import           Control.Monad.Logger (runStdoutLoggingT)

import qualified Network.Wai.Handler.Warp as Warp

import           Carma.EraGlonass.Server (serverApplicaton)
import           Carma.EraGlonass.Types (AppContext (..))
import           Carma.EraGlonass.Logger () -- MonadLoggerBus instance
import           Carma.Monad.LoggerBus.MonadLogger
import           Carma.Monad.LoggerBus
import           Carma.Monad.Thread
import           Carma.Monad.MVar


main :: IO ()
main = do
  cfg <- Conf.load [Conf.Required "app.cfg"]

  !(port :: Warp.Port) <- Conf.require cfg "port"
  !(host :: String)    <- Conf.lookupDefault "127.0.0.1" cfg "host"

  loggerBus' <- newEmptyMVar

  let appContext
        = AppContext
        { loggerBus = loggerBus'
        }

  flip runReaderT appContext $ do

    -- Running logger thread
    _ <- fork $ runStdoutLoggingT $ writeLoggerBusEventsToMonadLogger

    logInfo [qm| Running incoming server on http://{host}:{port}... |]

  runIncomingServer appContext port $ fromString host


runIncomingServer :: AppContext -> Warp.Port -> Warp.HostPreference -> IO ()
runIncomingServer appContext port host
  = Warp.runSettings warpSettings
  $ serverApplicaton appContext

  where warpSettings
          = Warp.defaultSettings
          & Warp.setPort port
          & Warp.setHost host
