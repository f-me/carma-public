-- This module handles requests for Nominatim responses.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Carma.NominatimMediator.StatisticsWriter
     ( statisticsWriterInit
     ) where

import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Time.Clock as Time
import           Data.Maybe (fromMaybe)
import           Text.InterpolatedString.QM

import           Control.Monad
import           Control.Monad.Reader.Class (MonadReader, asks)
import           Control.Concurrent.Lifted (takeMVar)

import           Carma.NominatimMediator.Types
import           Carma.NominatimMediator.Logger
import           Carma.Monad


-- Handler which waits for new statistics write and adds it to the app's state.
-- Supposed to be run in own thread.
statisticsWriterInit
  :: ( MonadReader AppContext m
     , MonadLoggerBus m
     , MonadMVar m -- To read next request and write real request
     , MonadIORefWithCounter m -- To read from cache
     )
  => m ()
statisticsWriterInit = do
  logInfo
    [qmb| Statistics writer is ready.
          Waiting for statistics writes... |]

  forever $ do
    -- Waiting for next write
    (utcTime, reqType, statResolve) <- asks statisticsBus >>= takeMVar
    let day = Time.utctDay utcTime

    logInfo
      [qms| Adding statistics for day {day} of request type "{reqType}"
            resolved to "{statResolve}"... |]

    dataRef <- asks statisticsData

    modifyIORefWithCounter' dataRef $ \daysMap ->
      let
        counterKey  = (reqType, statResolve)
        dayCounters = fromMaybe HM.empty $ M.lookup  day        daysMap
        counter     = fromMaybe 0        $ HM.lookup counterKey dayCounters
      in
        M.insert day (HM.insert counterKey (counter + 1) dayCounters) daysMap
