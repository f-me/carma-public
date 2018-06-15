-- This module synchonizes cache with file.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Carma.NominatimMediator.CacheSync
     ( CacheSyncFile (..)
     , cacheSyncInit
     , fillCacheWithSnapshot
     ) where

import           Prelude hiding (readFile, writeFile)

import qualified Data.Map as M
import           Data.Text (Text, toLower)
import           Data.Monoid ((<>))
import           Text.InterpolatedString.QM

import           Control.Monad
import qualified Control.Monad.State.Strict as S
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader.Class (MonadReader, asks)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Except (MonadError (throwError))

import           Carma.NominatimMediator.Types
import           Carma.NominatimMediator.Utils
import           Carma.NominatimMediator.Logger


data CacheSyncFile
   = OnlyResponsesCacheFile FilePath
   | OnlyStatisticsFile FilePath
   | ResponsesCacheAndStatisticsFiles FilePath FilePath
     deriving (Eq, Show)


-- Entry-point which depends on IO (don't know how to avoid this yet).
-- Checks if cache was updated and saves new cache snapshot to a file.
-- Supposed to be run in own thread.
cacheSyncInit
  :: (MonadReader AppContext m, MonadBaseControl IO m, MonadIO m)
  => Float -> CacheSyncFile -> m ()
cacheSyncInit syncIntervalInHours syncFile =
  prepare syncIntervalInHours syncFile
    >>= S.evalStateT (forever $ syncCache syncIntervalInHours syncFile)


prepare -- Log and provide initial state
  :: (MonadReader AppContext m, LoggerBusMonad m, IORefWithCounterMonad m)
  => Float -> CacheSyncFile -> m (Integer, Integer)
prepare syncIntervalInHours syncFile = do
  logInfo
    [qmb| Cache synchronizer is initialized.
          {fileLog}
          Interval between synchronizations is: \
            {floatShow syncIntervalInHours} hour(s). |]

  -- Just taking counter(s) to check if something changed since last check
  (,) <$> (fst <$> readNextCache) <*> (fst <$> readNextStatistics)

  where fileS :: FilePath -> Text -> Text
        fileS file title =
          [qmb| File "{file}" will be used to sync {title} snapshot. |]

        fileLog =
          case syncFile of
               OnlyResponsesCacheFile file -> fileS file "responses cache"
               OnlyStatisticsFile file -> fileS file "statistics"
               ResponsesCacheAndStatisticsFiles cacheFile statisticsFile ->
                 fileS cacheFile "responses cache" <> "\n" <>
                 fileS statisticsFile "statistics"


syncCache
  :: ( MonadReader AppContext m
     , S.MonadState (Integer, Integer) m -- Counters that indicates
                                         -- if something is changed
     , LoggerBusMonad m
     , IORefWithCounterMonad m -- Accessing responses cache or statistics data
     , DelayMonad m -- Waiting before checks
     , FileMonad m -- Saving snapshots to files
     )
  => Float -> CacheSyncFile -> m ()
syncCache syncIntervalInHours syncFile = do
  -- Waiting at start too, because at start cache snapshot is supposed to be
  -- just loaded from a file, and there would be nothing to sync.
  delay syncInterval

  logInfo [qn| Synchronizer goes... |]
  (prevCacheState,      prevStatisticsState) <- S.get
  (nextCacheState,      cacheSnapshot)       <- readNextCache
  (nextStatisticsState, statisticsSnapshot)  <- readNextStatistics

  let isCacheUpdated      = nextCacheState      /= prevCacheState
      isStatisticsUpdated = nextStatisticsState /= prevStatisticsState

  case syncFile of
       OnlyResponsesCacheFile file ->
         sync "Responses cache" "elements" isCacheUpdated file cacheSnapshot

       OnlyStatisticsFile file ->
         sync "Statistics" "days" isStatisticsUpdated file statisticsSnapshot

       ResponsesCacheAndStatisticsFiles cacheFile statisticsFile -> do
         sync "Responses cache" "elements"
              isCacheUpdated cacheFile cacheSnapshot

         sync "Statistics" "days"
              isStatisticsUpdated statisticsFile statisticsSnapshot

  when (isCacheUpdated || isStatisticsUpdated) $
    S.put (nextCacheState, nextStatisticsState)

  logInfo [qms| Synchronizer will wait for {syncIntervalInMinutes} minute(s)
                before next check... |]

  where
    sync
      :: ( MonadReader AppContext m
         , LoggerBusMonad m
         , FileMonad m
         , Show k
         , Show v
         )
      => Text
      -> Text
      -> Bool -- Something is updated since last check
      -> FilePath
      -> M.Map k v -- Snapshot
      -> m ()
    sync title sizeSfx isUpdated snapshotFile snapshot =
      if not isUpdated
         then logInfo [qms| {title} haven't changed since last check,
                            nothing to synchronize. |]

         else do logInfo [qms| {title} have been updated since last check,
                               saving current {toLower title} snapshot
                               ({M.size snapshot} {sizeSfx})
                               to a file "{snapshotFile}"... |]

                 writeFile snapshotFile $ show snapshot

                 logInfo [qms| {title} snapshot with
                               {M.size snapshot} {sizeSfx}
                               successfully saved
                               to a file "{snapshotFile}"... |]

    syncIntervalInSeconds = syncIntervalInHours * 60 * 60 :: Float
    syncInterval = round $ secondInMicroseconds * syncIntervalInSeconds :: Int

    syncIntervalInMinutes =
      round $ fromIntegral syncInterval / secondInMicroseconds / 60 :: Int


-- Checks if cache snapshot file (or/and statistics snapshot file) exists and
-- adds this snapshot to the cache otherwise (if a file doesn't exist) just does
-- nothing (except log messasge).
-- TODO implement filling statistics
fillCacheWithSnapshot
  :: ( MonadReader AppContext m
     , MonadError () m -- For interrupting, just ignore the exception
     , LoggerBusMonad m
     , IORefWithCounterMonad m
     , FileMonad m
     )
  => FilePath -> m ()
fillCacheWithSnapshot cacheFile = do
  logInfo [qm| Trying to read cache snapshot from file "{cacheFile}"... |]

  do x <- doesFileExist cacheFile

     unless x $ do
       logInfo [qms| Cache snapshot file "{cacheFile}" doesn't exist,
                     nothing to read. |]
       throwError ()

  logInfo [qms| Cache snapshot file "{cacheFile}" does exist,
                reading and parsing snapshot... |]

  (cacheSnapshot :: ResponsesCache) <- readFile cacheFile <&!> read

  logInfo
    [qmb| Cache snapshot is successfully read from file "{cacheFile}".
          Elements in snapshot: {M.size cacheSnapshot}.
          Merging it with existing cache... |]

  asks responsesCache >>= flip modifyIORefWithCounter' (M.union cacheSnapshot)


-- Helpers

readNextCache
  :: (MonadReader AppContext m, IORefWithCounterMonad m)
  => m (Integer, ResponsesCache)
readNextCache = asks responsesCache >>= readIORefWithCounter'

readNextStatistics
  :: (MonadReader AppContext m, IORefWithCounterMonad m)
  => m (Integer, StatisticsData)
readNextStatistics = asks statisticsData >>= readIORefWithCounter'
