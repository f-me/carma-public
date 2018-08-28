{-# LANGUAGE LambdaCase #-}

module Carma.Monad.LoggerBus.MonadLogger
     ( writeLoggerBusEventsToMonadLogger
     ) where

import           Control.Monad (forever)
import           Control.Monad.Logger

import           Carma.Monad.LoggerBus.Types
import           Carma.Monad.LoggerBus.Class


-- | Redirects events from logger bus to @MonadLogger@.
--
-- Supposed to be run in own thread (it's infinite).
writeLoggerBusEventsToMonadLogger :: (MonadLogger m, MonadLoggerBus m) => m ()
writeLoggerBusEventsToMonadLogger =
  forever $ readLog >>= \case
    LogForward loc src lvl msg -> monadLoggerLog loc src lvl msg

    LogMessage msgType msg ->
      case msgType of
           LogInfo  -> logInfoN  msg
           LogError -> logErrorN msg
