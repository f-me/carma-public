{-# LANGUAGE OverloadedStrings #-}

module Carma.NominatimMediator.CacheGC where

import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Concurrent

import           Carma.NominatimMediator.Types
import           Carma.NominatimMediator.Logger


-- Cleans outdated cached responses.
-- Supposed to be run in own thread.
cacheGCInit :: (LoggerBus m, MonadIO m) => AppContext -> m ()
cacheGCInit appCtx = forever $ do
  liftIO $ threadDelay cacheGCInterval
  logInfo appCtx "GC goes..."

  where
    cacheGCInterval = hour -- Every hour in microseconds
      where second = 1000 * 1000
            minute = 60 * second
            hour   = 60 * minute
