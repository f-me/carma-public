{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Carma.NominatimMediator.Utils.MonadStatisticsWriter where

import           Control.Concurrent.Lifted (fork, putMVar)
import           Control.Monad
import           Control.Monad.Reader.Class (MonadReader, asks)

import           Carma.NominatimMediator.Types
import           Carma.Monad


type MonadStatisticsWriter m
  = (Monad m, MonadReader AppContext m, MonadMVar m, MonadThread m)

writeStatistics
  :: MonadStatisticsWriter m
  => UTCTime -> RequestType -> StatisticResolve -> m ()
writeStatistics utcTime reqType resolve = do
  bus <- asks statisticsBus
  -- Forking for non-blocking writing
  void $ fork $ putMVar bus (utcTime, reqType, resolve)
