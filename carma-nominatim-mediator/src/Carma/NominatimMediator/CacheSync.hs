-- This module synchonizes cache with file.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Carma.NominatimMediator.CacheSync
     ( cacheSyncInit
     , fillCacheWithSnapshot
     ) where

import           Prelude hiding (readFile, writeFile)

import qualified Data.Map as M
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


cacheSyncInit
  :: (MonadReader AppContext m, MonadBaseControl IO m, MonadIO m)
  => Float -> FilePath -> m ()
cacheSyncInit syncIntervalInHours cacheFile =
  prepare syncIntervalInHours cacheFile
    >>= S.evalStateT (forever $ syncCache syncIntervalInHours cacheFile)


-- Checks if cache was updated and saves new cache snapshot to a file.
-- Supposed to be run in own thread.
prepare
  :: (MonadReader AppContext m, LoggerBusMonad m, IORefWithCounterMonad m)
  => Float -> FilePath -> m Integer
prepare syncIntervalInHours cacheFile = do
  logInfo
    [qmb| Cache synchronizer is initialized.
          Cache file "{cacheFile}" will be used to sync cache snapshot.
          Interval between synchronizations is: \
          {syncIntervalInHours} hour(s). |]

  fst <$> readNextCache


syncCache
  :: ( MonadReader AppContext m
     , S.MonadState Integer m
     , LoggerBusMonad m
     , IORefWithCounterMonad m
     , DelayMonad m
     , FileMonad m
     )
  => Float -> FilePath -> m ()
syncCache syncIntervalInHours cacheFile = do
  -- Waiting at start too, because at start cache snapshot is supposed to be
  -- just loaded from a file, and there would be nothing to sync.
  delay syncInterval

  logInfo [qn| Cache synchronizer goes... |]
  prevState <- S.get
  (nextState, cacheSnapshot) <- readNextCache

  if nextState == prevState
     then logInfo [qns| Cache haven't changed since last check,
                        nothing to synchronize. |]

     else do logInfo [qms| Cache have changed since last check,
                           saving current cache snapshot
                           ({M.size cacheSnapshot} elements)
                           to a file "{cacheFile}"... |]

             writeFile cacheFile $ show cacheSnapshot
             S.put nextState

             logInfo [qms| Cache snapshot with
                           {M.size cacheSnapshot} elements
                           successfully saved
                           to a file "{cacheFile}"... |]

  logInfo [qms| Cache synchronizer will wait for
                {syncIntervalInMinutes} minute(s)
                before next check... |]

  where
    syncIntervalInSeconds = syncIntervalInHours * 60 * 60 :: Float
    syncInterval = round $ secondInMicroseconds * syncIntervalInSeconds :: Int

    syncIntervalInMinutes =
      round $ fromIntegral syncInterval / secondInMicroseconds / 60 :: Int



-- Checks if cache snapshot file exists and adds this snapshot to the cache
-- otherwise (if a file doesn't exist) just does nothing (except log messasge).
fillCacheWithSnapshot
  :: ( MonadReader AppContext m
     , MonadError () m -- For interrupting, just ignore the exception
     , LoggerBusMonad m
     , IORefWithCounterMonad m
     , FileMonad m
     )
  => FilePath
  -> m ()
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
