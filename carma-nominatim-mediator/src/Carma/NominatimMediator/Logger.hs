-- This module handles logging messages.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Carma.NominatimMediator.Logger where

import qualified Data.Text as T
import           Text.InterpolatedString.QM

import           Control.Monad
import           Control.Monad.Reader.Class (MonadReader, reader)
import           Control.Monad.Logger (MonadLogger, logInfoN, logErrorN)

import           Carma.NominatimMediator.Types
import           Carma.NominatimMediator.Utils


class MonadReader AppContext m => LoggerBusMonad m where
  logInfo  :: T.Text -> m ()
  logError :: T.Text -> m ()
  readLog  :: m LogMessage

instance ( MonadReader AppContext m
         , MVarMonad m
         , TimeMonad m
         ) => LoggerBusMonad m
         where

  logInfo msg = do
    utc <- getCurrentTime
    loggerBus' <- reader loggerBus
    putMVar loggerBus' $ LogMessage LogInfo
      [qms| [{formatTime utc} UTC] {msg} |]

  logError msg = do
    utc <- getCurrentTime
    loggerBus' <- reader loggerBus
    putMVar loggerBus' $ LogMessage LogError
      [qms| [{formatTime utc} UTC] {msg} |]

  readLog = reader loggerBus >>= takeMVar


-- Writes log messages somewhere.
-- Supposed to be run in own thread.
loggerInit
  :: (MonadReader AppContext m, MonadLogger m, LoggerBusMonad m) => m ()
loggerInit = forever $ do
  LogMessage msgType msg <- readLog

  case msgType of
       LogInfo  -> logInfoN  msg
       LogError -> logErrorN msg
