{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Carma.NominatimMediator.Logger where

import qualified Data.Text as T
import qualified Data.Time.Clock as Time
import qualified Data.Time.Format as Time
import           Text.InterpolatedString.QM

import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (MonadLogger, logInfoN, logErrorN)
import           Control.Concurrent.MVar

import           Carma.NominatimMediator.Types


class Monad m => LoggerBus m where
  logInfo  :: AppContext -> T.Text -> m ()
  logError :: AppContext -> T.Text -> m ()

instance (Monad m, MonadIO m) => LoggerBus m where
  logInfo appCtx msg = liftIO $ do
    utc <- Time.getCurrentTime
    loggerBus appCtx `putMVar` LogMessage LogInfo
      [qms| [{Time.formatTime Time.defaultTimeLocale loggerTimeFormat utc} UTC]
            {msg} |]

  logError appCtx msg = liftIO $ do
    utc <- Time.getCurrentTime
    loggerBus appCtx `putMVar` LogMessage LogError
      [qms| [{Time.formatTime Time.defaultTimeLocale loggerTimeFormat utc} UTC]
            {msg} |]

loggerTimeFormat :: String
loggerTimeFormat = "%Y-%m-%d %H:%M:%S"


-- Writes log messages somewhere.
-- Supposed to be run in own thread.
loggerInit :: (MonadLogger m, MonadIO m) => AppContext -> m ()
loggerInit appCtx = forever $ do
  (LogMessage msgType msg) <- liftIO $ takeMVar $ loggerBus appCtx

  case msgType of
       LogInfo  -> logInfoN  msg
       LogError -> logErrorN msg
