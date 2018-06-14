{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Carma.NominatimMediator.Utils.StatisticsWriterMonad where

import           Data.Time.Clock (UTCTime)

import           Control.Monad.Reader.Class (MonadReader, asks)

import           Carma.NominatimMediator.Types
import           Carma.NominatimMediator.Utils


class Monad m => StatisticsWriterMonad m where
  writeStatistics :: UTCTime -> RequestType -> StatisticResolve -> m ()

instance (Monad m, MonadReader AppContext m, MVarMonad m) =>
         StatisticsWriterMonad m
         where
  writeStatistics utcTime reqType resolve =
    asks statisticsBus >>= flip putMVar (utcTime, reqType, resolve)
