{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Carma.NominatimMediator.Utils.MonadStatisticsWriter where

import           Control.Monad
import           Control.Monad.Reader.Class (MonadReader, asks)

import           Carma.NominatimMediator.Types
import           Carma.Monad


class Monad m => MonadStatisticsWriter m where
  writeStatistics :: UTCTime -> RequestType -> StatisticResolve -> m ()

instance (Monad m, MonadReader AppContext m, MonadMVar m, MonadThread m) =>
         MonadStatisticsWriter m
         where
  writeStatistics utcTime reqType resolve = do
    bus <- asks statisticsBus
    -- Forking for non-blocking writing
    void $ fork $ putMVar bus (utcTime, reqType, resolve)
