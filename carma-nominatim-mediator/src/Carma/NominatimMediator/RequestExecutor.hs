-- This module handles requests for Nominatim responses.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Carma.NominatimMediator.RequestExecutor where

import qualified Data.Map as M
import qualified Data.Time.Clock as Time
import           Text.InterpolatedString.QM

import           Control.Monad
import qualified Control.Monad.Trans.State.Strict as S
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Concurrent

import           Servant.Client

import           Carma.NominatimMediator.Types
import           Carma.NominatimMediator.Utils
import           Carma.NominatimMediator.Logger


-- Requests queue.
-- Supposed to be run in own thread.
-- It writes response to provided `MVar`.
requestExecutorInit
  :: (LoggerBus m, IORefWithCounterM m, MonadIO m) => AppContext -> m ()
requestExecutorInit appCtx = do
  -- First request also will be checked for interval
  -- notwithstanding there isn't previous request,
  -- this is okay because it solves possible case when
  -- this service is restarted quickly just after previous request
  -- before restart and interval could be smaller than required.
  -- This initial state contains request counter and time of last response.
  initialState <- liftIO Time.getCurrentTime <&> ((0 :: Integer),)

  logInfo appCtx [qn| Request executor is ready and waiting for requests... |]

  flip S.evalStateT initialState $ forever $ do
    -- Waiting for next request
    requestArgs@(reqParams, _, responseBus) <-
      liftIO $ takeMVar $ requestExecutorBus appCtx

    -- Increasing request counter
    S.modify' $ \(a, b) -> (a + 1, b)

    withCounter $ \n ->
      logInfo appCtx [qm| Executing request #{n} with params: {reqParams}... |]

    -- Checking if there's cached response by this request params
    cachedResponse <-
      M.lookup reqParams <$> readIORefWithCounter (responsesCache appCtx)

    case cachedResponse of
         -- Nothing found in cache for this request, requesting it
         Nothing -> request requestArgs

         -- Found cached result, using it to return to user immediately
         Just (_, x) -> do
           withCounter $ \n ->
             logInfo appCtx [qms| Response for request #{n}
                                  by params {reqParams} is taken from cache. |]

           liftIO $ putMVar responseBus $ Right x

  where
    -- Minimum interval is one second, making it little more safe
    intervalBetweenRequests = round $ secondInMicroseconds * 1.5

    intervalBetweenRequestsInSeconds =
      fromIntegral intervalBetweenRequests / secondInMicroseconds

    withCounter = (S.gets fst >>=)

    -- Real request, when nothing found in cache.
    -- Successful request will be added to cache.
    request (reqParams, req, responseBus) = do
      -- Handling minimal invervals between requests
      do lastTime <- S.gets snd
         curTime  <- liftIO Time.getCurrentTime

         let timeDiffInMicroseconds = round $ secondInMicroseconds * x :: Int
               where
                 x = fromRational
                   $ toRational
                   $ Time.diffUTCTime curTime lastTime

         when (timeDiffInMicroseconds < intervalBetweenRequests) $ do
           let waitTime = intervalBetweenRequests - timeDiffInMicroseconds
               waitTimeInSeconds = fromIntegral waitTime / secondInMicroseconds

           withCounter $ \n ->
             logInfo appCtx [qms| Request #{n} by params {reqParams}
                                  is delayed by {waitTimeInSeconds} seconds
                                  to satisfy interval between requests
                                  which is {intervalBetweenRequestsInSeconds}
                                  seconds... |]

           liftIO $ threadDelay waitTime

      result <- liftIO $ runClientM req $ clientEnv appCtx

      case result of
           Left e -> do
             withCounter $ \n ->
               logError appCtx [qms| Request #{n} by params {reqParams}
                                     is failed with exception: {e}. |]

             liftIO Time.getCurrentTime >>= S.modify' . fmap . const

           Right x -> do
             withCounter $ \n ->
               logInfo appCtx
                 [qms| Request #{n} by params {reqParams} is succeeded,
                       adding result to the cache... |]

             utc <- liftIO Time.getCurrentTime
             S.modify' $ fmap $ const utc

             responsesCache appCtx `modifyIORefWithCounter'`
               M.insert reqParams (utc, x)

      -- Sending response back
      liftIO $ putMVar responseBus result
