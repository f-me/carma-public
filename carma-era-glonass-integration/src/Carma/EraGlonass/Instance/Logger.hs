{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}

module Carma.EraGlonass.Instance.Logger () where

import           Text.InterpolatedString.QM
import           Data.ByteString (ByteString)

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Reader (ReaderT, ask, asks)
import           Control.Monad.Logger
import           Control.Concurrent.STM.TQueue

import           System.Log.FastLogger (fromLogStr)

import           Database.Persist.Sql (SqlBackend)

import           Carma.EraGlonass.Logger.LoggerForward
import           Carma.EraGlonass.Types (AppContext (loggerBus))
import           Carma.Monad.LoggerBus.Helpers (formatTime)
import           Carma.Monad.LoggerBus.Types
import           Carma.Monad.LoggerBus as LB
import           Carma.Monad.Clock
import           Carma.Monad.STM


instance ( Monad m
         , MonadSTM m
         , MonadClock m
         ) => MonadLoggerBus (ReaderT AppContext m)
         where

  logDebug msg = asks loggerBus >>= lift . flip (genericTQueueLog LogDebug) msg
  logInfo  msg = asks loggerBus >>= lift . flip (genericTQueueLog LogInfo ) msg
  logWarn  msg = asks loggerBus >>= lift . flip (genericTQueueLog LogWarn ) msg
  logError msg = asks loggerBus >>= lift . flip (genericTQueueLog LogError) msg
  readLog      = asks loggerBus >>= lift . genericTQueueReadLog


-- | When @AppContext@ isn't constructed yet but you need the logger bus.
instance ( Monad m
         , MonadSTM m
         , MonadClock m
         ) => MonadLoggerBus (ReaderT (TQueue LogMessage) m)
         where

  logDebug msg = ask >>= lift . flip (genericTQueueLog LogDebug) msg
  logInfo  msg = ask >>= lift . flip (genericTQueueLog LogInfo ) msg
  logWarn  msg = ask >>= lift . flip (genericTQueueLog LogWarn ) msg
  logError msg = ask >>= lift . flip (genericTQueueLog LogError) msg
  readLog      = ask >>= lift . genericTQueueReadLog


instance ( Monad m
         , MonadLoggerBus m
         ) => MonadLoggerBus (ReaderT SqlBackend m)
         where

  logDebug = lift . LB.logDebug
  logInfo  = lift . LB.logInfo
  logWarn  = lift . LB.logWarn
  logError = lift . LB.logError
  readLog  = lift   LB.readLog



instance ( Monad m
         , MonadSTM m
         , MonadClock m
         ) => MonadLoggerBus (LoggerForward m) where

  logDebug x = askLoggerForward >>= lift . flip (genericTQueueLog LogDebug) x
  logInfo  x = askLoggerForward >>= lift . flip (genericTQueueLog LogInfo ) x
  logWarn  x = askLoggerForward >>= lift . flip (genericTQueueLog LogWarn ) x
  logError x = askLoggerForward >>= lift . flip (genericTQueueLog LogError) x
  readLog    = askLoggerForward >>= lift . genericTQueueReadLog


instance ( Monad m
         , MonadSTM m
         , MonadClock m
         ) => MonadLogger (LoggerForward m) where

  monadLoggerLog loc src lvl msg = do
    !utc <- lift getCurrentTime
    loggerBus' <- askLoggerForward

    lift
      $ atomically
      $ writeTQueue loggerBus'
      $ LogForward loc src lvl
      $ toLogStr ([qms| [{formatTime utc} UTC]
                        {fromLogStr $ toLogStr msg} |] :: ByteString)
