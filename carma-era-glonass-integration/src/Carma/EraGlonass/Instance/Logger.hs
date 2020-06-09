{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, QuasiQuotes, BangPatterns #-}

module Carma.EraGlonass.Instance.Logger () where

import           Text.InterpolatedString.QM
import           Data.ByteString (ByteString)

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Reader (ReaderT, ask, asks)
import           Control.Monad.Logger
import           Control.Concurrent.STM.TQueue

import           Database.Persist.Sql (SqlBackend)

import           Carma.EraGlonass.Logger.LoggerForward
import           Carma.EraGlonass.Types.AppContext (AppContext (loggerBus))
import           Carma.Monad.LoggerBus.Helpers (formatTime)
import           Carma.Monad.LoggerBus.Types
import           Carma.Monad.LoggerBus as LB
import           Carma.Monad.Clock
import           Carma.Monad.STM


-- | Regular monad of this microservice.
instance ( Monad m
         , MonadSTM m
         , MonadClock m
         ) => MonadLoggerBus (ReaderT AppContext m)
         where

  logDebug msg =
    asks loggerBus >>= lift . flip (genericTQueueLog mempty LogDebug) msg
  logDebugS src msg =
    asks loggerBus >>= lift . flip (genericTQueueLog src LogDebug) msg

  logInfo msg =
    asks loggerBus >>= lift . flip (genericTQueueLog mempty LogInfo) msg
  logInfoS src msg =
    asks loggerBus >>= lift . flip (genericTQueueLog src LogInfo) msg

  logWarn msg =
    asks loggerBus >>= lift . flip (genericTQueueLog mempty LogWarn) msg
  logWarnS src msg =
    asks loggerBus >>= lift . flip (genericTQueueLog src LogWarn) msg

  logError msg =
    asks loggerBus >>= lift . flip (genericTQueueLog mempty LogError) msg
  logErrorS src msg =
    asks loggerBus >>= lift . flip (genericTQueueLog src LogError) msg

  readLog = asks loggerBus >>= lift . genericTQueueReadLog


-- | When @AppContext@ isn't constructed yet but you need the logger bus.
instance ( Monad m
         , MonadSTM m
         , MonadClock m
         ) => MonadLoggerBus (ReaderT (TQueue LogMessage) m)
         where

  logDebug msg =
    ask >>= lift . flip (genericTQueueLog mempty LogDebug) msg
  logDebugS src msg =
    ask >>= lift . flip (genericTQueueLog src LogDebug) msg

  logInfo msg =
    ask >>= lift . flip (genericTQueueLog mempty LogInfo) msg
  logInfoS src msg =
    ask >>= lift . flip (genericTQueueLog src LogInfo) msg

  logWarn msg =
    ask >>= lift . flip (genericTQueueLog mempty LogWarn) msg
  logWarnS src msg =
    ask >>= lift . flip (genericTQueueLog src LogWarn) msg

  logError msg =
    ask >>= lift . flip (genericTQueueLog mempty LogError) msg
  logErrorS src msg =
    ask >>= lift . flip (genericTQueueLog src LogError) msg

  readLog = ask >>= lift . genericTQueueReadLog


-- | Wrapped with __persistent__ SQL transaction.
instance ( Monad m
         , MonadLoggerBus m
         ) => MonadLoggerBus (ReaderT SqlBackend m)
         where

  logDebug      = lift . LB.logDebug
  logDebugS src = lift . LB.logDebugS src

  logInfo       = lift . LB.logInfo
  logInfoS  src = lift . LB.logInfoS  src

  logWarn       = lift . LB.logWarn
  logWarnS  src = lift . LB.logWarnS  src

  logError      = lift . LB.logError
  logErrorS src = lift . LB.logErrorS src

  readLog       = lift   LB.readLog



-- | Forwarding log messages somewhere else.
instance ( Monad m
         , MonadSTM m
         , MonadClock m
         ) => MonadLoggerBus (LoggerForward m) where

  logDebug msg =
    askLoggerForward >>= lift . flip (genericTQueueLog mempty LogDebug) msg
  logDebugS src msg =
    askLoggerForward >>= lift . flip (genericTQueueLog src LogDebug) msg

  logInfo msg =
    askLoggerForward >>= lift . flip (genericTQueueLog mempty LogInfo) msg
  logInfoS src msg =
    askLoggerForward >>= lift . flip (genericTQueueLog src LogInfo) msg

  logWarn msg =
    askLoggerForward >>= lift . flip (genericTQueueLog mempty LogWarn) msg
  logWarnS src msg =
    askLoggerForward >>= lift . flip (genericTQueueLog src LogWarn) msg

  logError msg =
    askLoggerForward >>= lift . flip (genericTQueueLog mempty LogError) msg
  logErrorS src msg =
    askLoggerForward >>= lift . flip (genericTQueueLog src LogError) msg

  readLog = askLoggerForward >>= lift . genericTQueueReadLog


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
