-- This module handles requests for Nominatim responses.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Carma.NominatimMediator.RequestExecutor
     ( requestExecutorInit
     ) where

import           Data.Monoid ((<>))
import qualified Data.Map as M
import qualified Data.Time.Clock as Time
import           Text.InterpolatedString.QM

import           Control.Monad
import qualified Control.Monad.State.Strict as S
import           Control.Monad.Reader.Class (MonadReader, asks)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Concurrent (MVar)

import           Servant.Client (ClientM, ServantError)

import           Carma.NominatimMediator.Types
import           Carma.NominatimMediator.Utils
import           Carma.NominatimMediator.Logger


type RealRequestQueue
   = MVar ( Integer -- Countered number of request (for logging)
          , RequestParams
          , ClientM Response -- Request monad to execute
          , MVar (Either ServantError Response) -- Response bus
          )


-- Requests queue.
-- Supposed to be run in own thread.
-- It writes response to provided `MVar`.
requestExecutorInit
  :: (MonadReader AppContext m, MonadBaseControl IO m, MonadIO m)
  => Float -> m ()
requestExecutorInit nominatimReqGapInSeconds = do
  realRequestQueue <- newEmptyMVar

  (_, requestHandlerForkWaiter) <-
    forkWithWaitBus $ flip S.evalStateT 0 $ forever $
      handleRequest realRequestQueue

  (_, realRequestHandlerForkWaiter) <-
    -- First request also will be checked for interval
    -- (becase using current time here as initial state)
    -- notwithstanding there isn't previous request,
    -- this is okay because it solves possible case when
    -- this service is restarted quickly just after previous request
    -- before restart and interval could be smaller than required.
    getCurrentTime >>= \utc ->
      forkWithWaitBus $ flip S.evalStateT utc $ forever $
        handleRealRequest nominatimReqGapInSeconds realRequestQueue

  logInfo
    [qmb| Request executor is ready.
          Gap between requests is {nominatimReqGapInSeconds} second(s).
          Waiting for requests... |]

  -- Block until nested threads is done (they usually never ends).
  mapM_ takeMVar [requestHandlerForkWaiter, realRequestHandlerForkWaiter]


handleRequest
  :: ( MonadReader AppContext m
     , S.MonadState Integer m -- Request counter
     , LoggerBusMonad m
     , MVarMonad m -- To read next request and write real request
     , IORefWithCounterMonad m -- To read from cache
     )
  => RealRequestQueue -> m ()
handleRequest realRequestQueue = do
  -- Waiting for next request
  (reqParams, reqMonad, responseBus) <- asks requestExecutorBus >>= takeMVar

  S.modify' (+1) -- Increasing request counter
  n <- S.get -- Current countered number of request
  logInfo [qm| Executing request #{n} with params: {reqParams}... |]

  -- Checking if there's cached response by this request params
  cachedResponse <-
    M.lookup reqParams <$> (asks responsesCache >>= readIORefWithCounter)

  case cachedResponse of
       -- Nothing found in cache for this request,
       -- adding task to real requests queue.
       Nothing -> do
         logInfo [qms| Response for request #{n} by params {reqParams}
                       not found in cache, sending this request
                       to the real requests queue...|]

         putMVar realRequestQueue (n, reqParams, reqMonad, responseBus)

       -- Found cached result, using it to return to user immediately
       Just (_, x) -> do
         logInfo [qms| Response for request #{n} by params {reqParams}
                       is taken from cache. |]

         putMVar responseBus $ Right x


-- Real request handling, when nothing found in cache.
-- Successful request will be added to the cache
-- (except reverse search requests when it is disabled in app config,
-- it makes sense because requests is almost always unique).
handleRealRequest
  :: ( MonadReader AppContext m
     , S.MonadState Time.UTCTime m -- Time of last request (for intervals)
     , LoggerBusMonad m
     , TimeMonad m -- Handle intervals by comparing time
     , DelayMonad m -- Waiting intervals
     , MVarMonad m -- Reading next request
     , IORefWithCounterMonad m -- Writing to cache
     , ServantClientMonad m -- Real client requests
     )
  => Float -> RealRequestQueue -> m ()
handleRealRequest nominatimReqGapInSeconds realRequestQueue = do
  -- Waiting for next real request
  (n, reqParams, reqMonad, responseBus) <- takeMVar realRequestQueue

  -- Handling minimal invervals between requests.
  do lastTime <- S.get
     curTime  <- getCurrentTime

     let timeDiffInMicroseconds :: Int
         timeDiffInMicroseconds = round $ secondInMicroseconds * x
           where x = fromRational
                   $ toRational
                   $ Time.diffUTCTime curTime lastTime

     when (timeDiffInMicroseconds < intervalBetweenRequests) $ do
       let waitTime = intervalBetweenRequests - timeDiffInMicroseconds
           waitTimeInSeconds = fromIntegral waitTime / secondInMicroseconds

       logInfo [qms| Request #{n} by params {reqParams}
                     is delayed by {waitTimeInSeconds} second(s)
                     to satisfy interval between requests
                     which is {intervalBetweenRequestsInSeconds}
                     second(s)... |]

       delay waitTime

  result <- asks clientEnv >>= runClientM reqMonad

  case result of
       Left e -> do
         logError [qms| Request #{n} by params {reqParams}
                        is failed with exception: {e}. |]

         getCurrentTime >>= S.put

       Right x -> do
         isCacheDisabledForRevSearch <- asks cacheForRevSearchIsDisabled
         let isRevSearchRequest = requestType reqParams == ReverseSearch
             isResultGoingToCache =
               not $ isRevSearchRequest && isCacheDisabledForRevSearch

         logInfo $
           [qm| Request #{n} by params {reqParams} is succeeded,\ |]
             <> if isResultGoingToCache
                   then [qns| adding result to the cache... |]
                   else [qns| result won't be added to the cache because
                              it is disabled for this type of requests. |]

         utc <- getCurrentTime
         S.put utc

         when isResultGoingToCache $
           asks responsesCache >>=
             flip modifyIORefWithCounter' (M.insert reqParams (utc, x))

  -- Sending response back
  putMVar responseBus result

  where
    -- Minimum interval in microseconds
    intervalBetweenRequests =
      round $ secondInMicroseconds * nominatimReqGapInSeconds :: Int

    intervalBetweenRequestsInSeconds =
      fromIntegral intervalBetweenRequests / secondInMicroseconds :: Float
