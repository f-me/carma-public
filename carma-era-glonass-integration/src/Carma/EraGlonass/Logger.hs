{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Carma.EraGlonass.Logger where

import           Control.Monad.Reader.Class (MonadReader, asks)

import           Carma.EraGlonass.Types (AppContext (loggerBus))
import           Carma.Monad.LoggerBus
import           Carma.Monad.Thread
import           Carma.Monad.Clock
import           Carma.Monad.MVar


instance ( Monad m
         , MonadReader AppContext m
         , MonadMVar m
         , MonadClock m
         , MonadThread m
         ) => MonadLoggerBus m
         where

  logInfo  msg = asks loggerBus >>= flip logInfoImpl  msg
  logError msg = asks loggerBus >>= flip logErrorImpl msg
  readLog      = asks loggerBus >>= readLogImpl
