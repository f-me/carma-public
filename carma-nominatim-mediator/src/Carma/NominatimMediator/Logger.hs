-- This module handles logging messages.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Carma.NominatimMediator.Logger where

import qualified Data.Text as T
import qualified Data.Time.Clock as Time
import           Text.InterpolatedString.QM

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad.Logger (MonadLogger, logInfoN, logErrorN)
import           Control.Concurrent.MVar

import           Servant (Handler)

import           Carma.NominatimMediator.Types
import           Carma.NominatimMediator.Utils


class Monad m => LoggerBusMonad m where
  logInfo  :: AppContext -> T.Text -> m ()
  logError :: AppContext -> T.Text -> m ()
  readLog  :: AppContext -> m LogMessage

instance LoggerBusMonad IO where
  logInfo appCtx msg = do
    utc <- Time.getCurrentTime
    loggerBus appCtx `putMVar` LogMessage LogInfo
      [qms| [{formatTime utc} UTC] {msg} |]

  logError appCtx msg = do
    utc <- Time.getCurrentTime
    loggerBus appCtx `putMVar` LogMessage LogError
      [qms| [{formatTime utc} UTC] {msg} |]

  readLog = loggerBus ? takeMVar

instance LoggerBusMonad Handler where
  logInfo  appCtx = liftIO . logInfo  appCtx
  logError appCtx = liftIO . logError appCtx
  readLog  appCtx = liftIO $ readLog  appCtx

instance (LoggerBusMonad m, MonadTrans t, Monad (t m)) => LoggerBusMonad (t m)
  where
  logInfo  appCtx = lift . logInfo  appCtx
  logError appCtx = lift . logError appCtx
  readLog  appCtx = lift $ readLog  appCtx


-- Writes log messages somewhere.
-- Supposed to be run in own thread.
loggerInit :: (MonadLogger m, LoggerBusMonad m) => AppContext -> m ()
loggerInit appCtx = forever $ do
  (LogMessage msgType msg) <- readLog appCtx

  case msgType of
       LogInfo  -> logInfoN  msg
       LogError -> logErrorN msg
