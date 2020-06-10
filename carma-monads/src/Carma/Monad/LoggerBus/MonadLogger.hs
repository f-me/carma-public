{-# LANGUAGE LambdaCase #-}

module Carma.Monad.LoggerBus.MonadLogger
     ( writeLoggerBusEventsToMonadLogger
     ) where

import           Control.Monad (forever)
import           Control.Monad.Logger

import           Carma.Monad.LoggerBus.Types


-- | Redirects events from logger bus to @MonadLogger@.
--
-- Supposed to be run in own thread (it's infinite).
writeLoggerBusEventsToMonadLogger :: MonadLogger m => m LogMessage -> m ()
writeLoggerBusEventsToMonadLogger readLog =
  forever $ readLog >>= \case
    LogForward loc src lvl msg -> monadLoggerLog loc src lvl msg

    LogMessage src msgType msg ->
      case msgType of
           LogDebug -> logDebugNS src msg
           LogInfo  -> logInfoNS  src msg
           LogWarn  -> logWarnNS  src msg
           LogError -> logErrorNS src msg
