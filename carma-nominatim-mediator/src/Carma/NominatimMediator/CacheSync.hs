-- This module synchonizes cache with file.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

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

import           Carma.NominatimMediator.Types
import           Carma.NominatimMediator.Utils
import           Carma.NominatimMediator.Logger ()
import           Carma.Monad
import           Carma.Utils.Operators


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
  :: (MonadReader AppContext m, MonadLoggerBus m, MonadIORefWithCounter m)
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
     , MonadLoggerBus m
     , MonadIORefWithCounter m -- Accessing responses cache or statistics data
     , MonadDelay m -- Waiting before checks
     , MonadFile m -- Saving snapshots to files
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
         , MonadLoggerBus m
         , MonadFile m
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
fillCacheWithSnapshot
  :: ( MonadReader AppContext m
     , MonadLoggerBus m
     , MonadIORefWithCounter m
     , MonadFile m
     )
  => CacheSyncFile -> m ()
fillCacheWithSnapshot = \case
  OnlyResponsesCacheFile file -> do
    fileExists <- checkFileExistence "Cached responses" file
    when fileExists $ loadCacheSnapshot file

  OnlyStatisticsFile file -> do
    fileExists <- checkFileExistence "Statistics" file
    when fileExists $ loadStatisticsSnapshot file

  ResponsesCacheAndStatisticsFiles cacheFile statisticsFile -> do
    cacheFileExists <- checkFileExistence "Cached responses" cacheFile
    when cacheFileExists $ loadCacheSnapshot cacheFile

    statisticsFileExists <- checkFileExistence "Statistics" statisticsFile
    when statisticsFileExists $ loadStatisticsSnapshot statisticsFile

  where
    checkFileExistence
      :: (MonadLoggerBus m, MonadFile m)
      => Text -> FilePath -> m Bool
    checkFileExistence title file = do
      logInfo
        [qm| Trying to read {toLower title} snapshot from file "{file}"... |]

      x <- doesFileExist file
      x <$ if x then logInfo [qms| {title} snapshot file "{file}" does exist,
                                   reading and parsing snapshot... |]

                else logInfo [qms| {title} snapshot file "{file}" doesn't exist,
                                   nothing to read. |]

    loadCacheSnapshot
      :: ( MonadReader AppContext m
         , MonadLoggerBus m
         , MonadFile m
         , MonadIORefWithCounter m
         )
      => FilePath -> m ()
    loadCacheSnapshot file = do
      (snapshot :: ResponsesCache) <- readFile file <&!> read
      logInfo $ loadedMsg "Cached responses" "Elements" file snapshot
      asks responsesCache >>= flip modifyIORefWithCounter' (M.union snapshot)

    loadStatisticsSnapshot
      :: ( MonadReader AppContext m
         , MonadLoggerBus m
         , MonadFile m
         , MonadIORefWithCounter m
         )
      => FilePath -> m ()
    loadStatisticsSnapshot file = do
      (snapshot :: StatisticsData) <- readFile file <&!> read
      logInfo $ loadedMsg "Statistics" "Days" file snapshot
      asks statisticsData >>= flip modifyIORefWithCounter' (M.union snapshot)

    loadedMsg :: Text -> Text -> FilePath -> M.Map k v -> Text
    loadedMsg title sizeTitle file snapshot =
      [qmb| {title} snapshot is successfully read from file "{file}".
            {sizeTitle} in the snapshot: {M.size snapshot}.
            Merging it with existing data... |]


-- Helpers

readNextCache
  :: (MonadReader AppContext m, MonadIORefWithCounter m)
  => m (Integer, ResponsesCache)
readNextCache = asks responsesCache >>= readIORefWithCounter'

readNextStatistics
  :: (MonadReader AppContext m, MonadIORefWithCounter m)
  => m (Integer, StatisticsData)
readNextStatistics = asks statisticsData >>= readIORefWithCounter'
