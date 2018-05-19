-- This modules claens outdated cached responses.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Carma.NominatimMediator.CacheGC where

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
cacheGCInit
  :: (LoggerBus m, IORefWithCounterM m, MonadIO m)
  => AppContext -> Float -> Float -> m ()
cacheGCInit appCtx gcIntervalInHours cacheItemLifetimeInHours = do
  logInfo appCtx
    [qmb| Cache garbage collector is initialized.
          Intervals between checks is {gcIntervalInHours} hour(s).
          Cached response lifetime is {cacheItemLifetimeInHours} hour(s). |]

  forever $ do
    logInfo appCtx [qn| Cache garbage collector goes... |]
    currentTime <- liftIO Time.getCurrentTime

    outdatedItems <-
      atomicModifyIORefWithCounter' (responsesCache appCtx) $
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
                         {cacheGCIntervalInMinutes} minute(s)
                         before next check... |]

    liftIO $ threadDelay cacheGCInterval

  where
    gcIntervalInSeconds = gcIntervalInHours * 60 * 60 :: Float
    cacheGCInterval = round $ secondInMicroseconds * gcIntervalInSeconds :: Int

    cacheGCIntervalInMinutes =
      round $ fromIntegral cacheGCInterval / secondInMicroseconds / 60 :: Int

    cacheItemLifetimeInSeconds = cacheItemLifetimeInHours * 60 * 60 :: Float
    cacheItemLifetime = round $ 24 * cacheItemLifetimeInSeconds :: Int
