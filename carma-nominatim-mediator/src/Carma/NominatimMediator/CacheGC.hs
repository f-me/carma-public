{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Carma.NominatimMediator.CacheGC where

import           Data.IORef
import           Data.Monoid ((<>))
import qualified Data.Map as M
import qualified Data.Time.Clock as Time
import qualified Data.Text as T
import           Text.InterpolatedString.QM

import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Concurrent

import           Carma.NominatimMediator.Types
import           Carma.NominatimMediator.Utils
import           Carma.NominatimMediator.Logger


-- Cleans outdated cached responses.
-- Supposed to be run in own thread.
cacheGCInit :: (LoggerBus m, MonadIO m) => AppContext -> m ()
cacheGCInit appCtx = forever $ do
  logInfo appCtx [qm| Cache garbage collector goes... |]
  currentTime <- liftIO Time.getCurrentTime

  outdatedItems <- liftIO $
    atomicModifyIORef' (responsesCache appCtx) $
      M.partition $ fst
                  ? Time.diffUTCTime currentTime
                  ? round
                  ? (<= cacheItemLifetime)

  when (M.size outdatedItems > 0) $
    logInfo appCtx [qms| These responses is outdated
                         and they were removed from cache.
                         They will be requested again next time.
                         Request params of responses
                         which were removed from cache:\
                         { T.pack $ mconcat
                         $ show ? ("\n  - " <>) <$> M.keys outdatedItems
                         } |]

  logInfo appCtx [qms| Cache garbage collector will wait for
                       {cacheGCIntervalInMinutes} minutes
                       before next check... |]

  liftIO $ threadDelay cacheGCInterval

  where
    cacheGCInterval = round $ secondInMicroseconds * 60 * 60 :: Int

    cacheGCIntervalInMinutes =
      round $ fromIntegral cacheGCInterval / secondInMicroseconds / 60 :: Int

    cacheItemLifetime = 24 * hourInSeconds :: Int
    hourInSeconds = 60 * 60 :: Int
