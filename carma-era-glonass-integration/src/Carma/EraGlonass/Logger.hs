{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}

module Carma.EraGlonass.Logger where

import           Text.InterpolatedString.QM
import           Data.ByteString (ByteString)

import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Reader (ReaderT, ask, asks)
import           Control.Monad.Logger

import           System.Log.FastLogger (fromLogStr)

import           Carma.EraGlonass.Logger.LoggerForward
import           Carma.EraGlonass.Types (AppContext (loggerBus))
import           Carma.Monad.LoggerBus.Helpers (formatTime)
import           Carma.Monad.LoggerBus.Types
import           Carma.Monad.LoggerBus
import           Carma.Monad.Thread
import           Carma.Monad.Clock
import           Carma.Monad.MVar


instance ( Monad m
         , MonadMVar m
         , MonadClock m
         , MonadThread m
         ) => MonadLoggerBus (ReaderT AppContext m)
         where

  logDebug msg = asks loggerBus >>= lift . flip (genericLogImpl LogDebug) msg
  logInfo  msg = asks loggerBus >>= lift . flip (genericLogImpl LogInfo ) msg
  logWarn  msg = asks loggerBus >>= lift . flip (genericLogImpl LogWarn ) msg
  logError msg = asks loggerBus >>= lift . flip (genericLogImpl LogError) msg
  readLog      = asks loggerBus >>= lift . readLogImpl


-- | When @AppContext@ isn't constructed yet but you need the logger bus.
instance ( Monad m
         , MonadMVar m
         , MonadClock m
         , MonadThread m
         ) => MonadLoggerBus (ReaderT (MVar LogMessage) m)
         where

  logDebug msg = ask >>= lift . flip (genericLogImpl LogDebug) msg
  logInfo  msg = ask >>= lift . flip (genericLogImpl LogInfo ) msg
  logWarn  msg = ask >>= lift . flip (genericLogImpl LogWarn ) msg
  logError msg = ask >>= lift . flip (genericLogImpl LogError) msg
  readLog      = ask >>= lift . readLogImpl


instance ( Monad m
         , MonadMVar m
         , MonadClock m
         , MonadThread m
         ) => MonadLoggerBus (LoggerForward m) where

  logDebug msg = askLoggerForward >>= lift . flip (genericLogImpl LogDebug) msg
  logInfo  msg = askLoggerForward >>= lift . flip (genericLogImpl LogInfo ) msg
  logWarn  msg = askLoggerForward >>= lift . flip (genericLogImpl LogWarn ) msg
  logError msg = askLoggerForward >>= lift . flip (genericLogImpl LogError) msg
  readLog      = askLoggerForward >>= lift . readLogImpl


instance ( Monad m
         , MonadMVar m
         , MonadClock m
         , MonadThread m
         ) => MonadLogger (LoggerForward m) where

  monadLoggerLog loc src lvl msg = do
    !utc <- lift getCurrentTime
    loggerBus' <- askLoggerForward

    void $ lift $ fork $ -- Forking for non-blocking writing to MVar
      putMVar loggerBus' $
        LogForward loc src lvl $
          toLogStr ([qms| [{formatTime utc} UTC]
                          {fromLogStr $ toLogStr msg} |] :: ByteString)
