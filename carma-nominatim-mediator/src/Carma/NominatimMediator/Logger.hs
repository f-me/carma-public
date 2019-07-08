{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances, FlexibleContexts #-}

-- | This module handles logging messages.
module Carma.NominatimMediator.Logger where

import           Control.Monad.Reader.Class (MonadReader, asks)

import           Carma.NominatimMediator.Types
import           Carma.Monad.LoggerBus.Types
import           Carma.Monad


instance ( Monad m
         , MonadReader AppContext m
         , MonadMVar m
         , MonadClock m
         , MonadThread m
         ) => MonadLoggerBus m
         where

  logDebug msg =
    asks loggerBus >>= flip (genericMVarLog mempty LogDebug) msg
  logDebugS src msg =
    asks loggerBus >>= flip (genericMVarLog src LogDebug) msg

  logInfo msg =
    asks loggerBus >>= flip (genericMVarLog mempty LogInfo) msg
  logInfoS src msg =
    asks loggerBus >>= flip (genericMVarLog src LogInfo) msg

  logWarn msg =
    asks loggerBus >>= flip (genericMVarLog mempty LogWarn) msg
  logWarnS src msg =
    asks loggerBus >>= flip (genericMVarLog src LogWarn) msg

  logError msg =
    asks loggerBus >>= flip (genericMVarLog mempty LogError) msg
  logErrorS src msg =
    asks loggerBus >>= flip (genericMVarLog src LogError) msg

  readLog = asks loggerBus >>= genericMVarReadLog
