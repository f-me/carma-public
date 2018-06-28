-- This module handles logging messages.

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Carma.NominatimMediator.Logger where

import           Control.Monad.Reader.Class (MonadReader, asks)

import           Carma.NominatimMediator.Types
import           Carma.Monad


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
