-- This module synchonizes cache with file.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Carma.NominatimMediator.CacheSync where

import qualified Data.Map as M
import           Text.InterpolatedString.QM

import           Control.Monad
import           Control.Monad.Trans.Except
import qualified Control.Monad.Trans.State.Strict as S
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Concurrent

import           System.Directory (doesFileExist)

import           Carma.NominatimMediator.Types
import           Carma.NominatimMediator.Utils
import           Carma.NominatimMediator.Logger


-- Checks if cache was updated and saves new cache snapshot to a file.
-- Supposed to be run in own thread.
cacheSyncInit
  :: (LoggerBus m, MonadIO m) => AppContext -> Float -> FilePath -> m ()
cacheSyncInit appCtx syncIntervalInHours cacheFile = do
  logInfo appCtx
    [qmb| Cache synchronizer is initialized.
          Cache file "{cacheFile}" will be used to sync cache snapshot.
          Interval between synchronizations is: \
          {syncIntervalInHours} hour(s). |]

  initialState <- fst <$> readNextCache

  flip S.evalStateT initialState $ forever $ do
    -- Waiting at start too, because at start cache snapshot is supposed to be
    -- just loaded from a file, and there would be nothing to sync.
    liftIO $ threadDelay syncInterval

    logInfo appCtx [qn| Cache synchronizer goes... |]
    prevState <- S.get
    (nextState, cacheSnapshot) <- readNextCache

    if nextState == prevState
       then logInfo appCtx [qms| Cache haven't changed since last check,
                                 nothing to synchronize. |]

       else do logInfo appCtx [qms| Cache have changed since last check,
                                    saving current cache snapshot
                                    ({M.size cacheSnapshot} elements)
                                    to a file "{cacheFile}"... |]

               liftIO $ writeFile cacheFile $ show cacheSnapshot
               S.put nextState

               logInfo appCtx [qms| Cache snapshot with
                                    {M.size cacheSnapshot} elements
                                    successfully saved
                                    to a file "{cacheFile}"... |]

    logInfo appCtx [qms| Cache synchronizer will wait for
                         {syncIntervalInMinutes} minute(s)
                         before next check... |]

  where
    syncIntervalInSeconds = syncIntervalInHours * 60 * 60 :: Float
    syncInterval = round $ secondInMicroseconds * syncIntervalInSeconds :: Int

    syncIntervalInMinutes =
      round $ fromIntegral syncInterval / secondInMicroseconds / 60 :: Int

    readNextCache = readIORefWithCounter' $ responsesCache appCtx


-- Checks if cache snapshot file exists and adds this snapshot to the cache
-- otherwise (if a file doesn't exist) just does nothing (except log messasge).
fillCacheWithSnapshot
  :: (LoggerBus m, IORefWithCounterM m, MonadIO m)
  => AppContext -> FilePath -> m ()
fillCacheWithSnapshot appCtx cacheFile = void $ runExceptT $ do
  logInfo appCtx
    [qm| Trying to read cache snapshot from file "{cacheFile}"... |]

  do x <- liftIO $ doesFileExist cacheFile

     unless x $ do
       logInfo appCtx [qms| Cache snapshot file "{cacheFile}" doesn't exist,
                            nothing to read. |]
       throwE ()

  logInfo appCtx [qms| Cache snapshot file "{cacheFile}" does exist,
                       reading and parsing snapshot... |]

  (cacheSnapshot :: ResponsesCache) <- liftIO $ readFile cacheFile <&!> read

  logInfo appCtx
    [qmb| Cache snapshot is successfully read from file "{cacheFile}".
          Elements in snapshot: {M.size cacheSnapshot}.
          Merging it with existing cache... |]

  responsesCache appCtx `modifyIORefWithCounter'` M.union cacheSnapshot
