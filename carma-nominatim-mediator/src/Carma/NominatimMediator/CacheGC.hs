-- This modules claens outdated cached responses.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Carma.NominatimMediator.CacheGC where

import           Data.Monoid ((<>))
import qualified Data.Map as M
import qualified Data.Time.Clock as Time
import qualified Data.Time.Calendar as Calendar
import qualified Data.Text as T
import           Text.InterpolatedString.QM
import           Data.Function ((&))

import           Control.Monad
import           Control.Monad.Reader.Class (MonadReader, asks)

import           Carma.NominatimMediator.Types
import           Carma.NominatimMediator.Utils
import           Carma.NominatimMediator.Logger ()
import           Carma.Monad
import           Carma.Utils.Operators


-- Cleans outdated cached responses and statistics for days older than N days
-- (specified in app config).
-- Supposed to be run in own thread.
cacheGCInit
  :: ( MonadReader AppContext m
     , MonadLoggerBus m
     , MonadIORefWithCounter m
     , MonadClock m
     , MonadDelay m
     )
  => Float
  -> Float
  -> Integer
  -> m ()
cacheGCInit gcIntervalInHours
            cacheItemLifetimeInHours
            statisticsLifetimeInDays = do
  logInfo
    [qmb| Cache garbage collector is initialized.
          Intervals between checks is {floatShow gcIntervalInHours} hour(s).
          Cached response lifetime is \
            {floatShow cacheItemLifetimeInHours} hour(s).
          Collected statistics of a day lifetime is \
            {statisticsLifetimeInDays} day(s). |]

  forever $ do
    logInfo [qn| Cache garbage collector goes... |]
    currentTime <- getCurrentTime

    -- Eliminating outdated cached responses and extracting them to use in log.
    outdatedItems <-
      asks responsesCache >>=
        flip atomicModifyIORefWithCounter'
          (M.partition $ fst
                       ? Time.diffUTCTime currentTime
                       ? round
                       ? (<= cacheItemLifetime))

    -- Eliminating outdated statistics days and extracting them to use in log.
    outdatedDaysOfStatistics <-
      asks statisticsData >>=
        flip atomicModifyIORefWithCounter'
          (M.partitionWithKey $ \day _ -> day
                              & Calendar.diffDays (Time.utctDay currentTime)
                              & (<= statisticsLifetimeInDays))

    -- If there's some eliminated outdated cached responses we're logging it.
    when (M.size outdatedItems > 0) $
      logInfo [qms| These responses is outdated
                    and they were removed from cache.
                    They will be requested again next time.
                    Request params of responses which were removed from cache:\
                    { T.pack $ mconcat
                    $ show ? ("\n  - " <>) <$> M.keys outdatedItems
                    } |]

    -- If there's some eliminated outdated statistics days we're logging it.
    when (M.size outdatedDaysOfStatistics > 0) $
      logInfo [qms| These collected days of statistics is outdated
                    and they were eliminated.
                    Days of collected statistics which were eliminated:\
                    { T.pack $ mconcat
                    $ show ? ("\n  - " <>) <$> M.keys outdatedDaysOfStatistics
                    } |]

    logInfo [qms| Cache garbage collector will wait for
                  {cacheGCIntervalInMinutes} minute(s)
                  before next check... |]

    delay cacheGCInterval

  where
    gcIntervalInSeconds = gcIntervalInHours * 60 * 60 :: Float
    cacheGCInterval = round $ secondInMicroseconds * gcIntervalInSeconds :: Int

    cacheGCIntervalInMinutes =
      round $ fromIntegral cacheGCInterval / secondInMicroseconds / 60 :: Int

    cacheItemLifetimeInSeconds = cacheItemLifetimeInHours * 60 * 60 :: Float
    cacheItemLifetime = round $ 24 * cacheItemLifetimeInSeconds :: Int
