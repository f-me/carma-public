-- This module handles logging messages.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}

module Carma.NominatimMediator.Logger where

import qualified Data.Text as T
import           Text.InterpolatedString.QM

import           Control.Monad
import           Control.Monad.Reader.Class (MonadReader, asks)
import           Control.Monad.Logger (MonadLogger, logInfoN, logErrorN)

import           Carma.NominatimMediator.Types
import           Carma.NominatimMediator.Utils
import           Carma.Monad


class Monad m => LoggerBusMonad m where
  logInfo  :: T.Text -> m ()
  logError :: T.Text -> m ()
  readLog  :: m LogMessage

instance ( Monad m
         , MonadReader AppContext m
         , MonadMVar m
         , MonadClock m
         , MonadThread m
         ) => LoggerBusMonad m
         where

  logInfo msg = do
    !utc <- getCurrentTime
    loggerBus' <- asks loggerBus

    void $ fork $ -- Forking for non-blocking writing to MVar
      putMVar loggerBus' $ LogMessage LogInfo
        [qm| [{formatTime utc} UTC] {msg} |]

  logError msg = do
    !utc <- getCurrentTime
    loggerBus' <- asks loggerBus

    void $ fork $ -- Forking for non-blocking writing to MVar
      putMVar loggerBus' $ LogMessage LogError
        [qm| [{formatTime utc} UTC] {msg} |]

  readLog = asks loggerBus >>= takeMVar


-- Writes log messages somewhere.
-- Supposed to be run in own thread.
loggerInit
  :: (MonadReader AppContext m, MonadLogger m, LoggerBusMonad m) => m ()
loggerInit = forever $ do
  LogMessage msgType msg <- readLog

  case msgType of
       LogInfo  -> logInfoN  msg
       LogError -> logErrorN msg
