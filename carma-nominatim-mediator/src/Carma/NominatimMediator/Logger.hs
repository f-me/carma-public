{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

-- | This module handles logging messages.
module Carma.NominatimMediator.Logger where

import           Data.Text (Text)
import           Control.Monad.Reader.Class (MonadReader, asks)
import           Control.Monad.Logger (LogSource)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.IO.Class (MonadIO)
import           Carma.NominatimMediator.Types
import           Carma.Monad.LoggerBus.Types
import           Carma.Monad.LoggerBus


type MonadLoggerBus m
  = (MonadIO m, MonadBaseControl IO m, MonadReader AppContext m)

logDebug, logInfo, logWarn, logError
  :: MonadLoggerBus m => Text -> m ()
logDebug = logHelper mempty LogDebug
logInfo = logHelper mempty LogInfo
logWarn = logHelper mempty LogWarn
logError = logHelper mempty LogError

logDebugS, logInfoS, logWarnS, logErrorS
  :: MonadLoggerBus m => LogSource -> Text -> m ()
logDebugS src = logHelper src LogDebug
logInfoS src = logHelper src LogInfo
logWarnS src = logHelper src LogWarn
logErrorS src = logHelper src LogError

logHelper
  :: MonadLoggerBus m
  => LogSource -> LogMessageType -> Text -> m ()
logHelper src lvl msg = do
  lb <- asks loggerBus
  genericMVarLog src lvl lb msg

readLog :: MonadLoggerBus m => m LogMessage
readLog = asks loggerBus >>= genericMVarReadLog
