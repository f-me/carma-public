-- This module handles requests for Nominatim responses.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Carma.NominatimMediator.RequestExecutor
     ( requestExecutorInit
     ) where

import qualified Data.Map as M
import qualified Data.Time.Clock as Time
import           Text.InterpolatedString.QM

import           Control.Monad
import qualified Control.Monad.State.Strict as S
import           Control.Monad.Reader.Class (MonadReader, asks)
import           Control.Monad.Base (MonadBase)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Concurrent (MVar)

import           Servant.Client (ClientM, ServantError)

import           Carma.NominatimMediator.Types
import           Carma.NominatimMediator.Utils
import           Carma.NominatimMediator.Logger


-- Requests queue.
-- Supposed to be run in own thread.
-- It writes response to provided `MVar`.
requestExecutorInit
  :: (MonadReader AppContext m, MonadBase IO m, MonadIO m)
  => Float -> m ()
requestExecutorInit nominatimReqGapInSeconds =
  prepare nominatimReqGapInSeconds
    >>= S.evalStateT (forever $ handleRequest nominatimReqGapInSeconds)


prepare
  :: (MonadReader AppContext m, LoggerBusMonad m, TimeMonad m)
  => Float -> m (Integer, Time.UTCTime)
prepare nominatimReqGapInSeconds = do
  logInfo
    [qmb| Request executor is ready.
          Gap between requests is {nominatimReqGapInSeconds} second(s).
          Waiting for requests... |]

  -- First request also will be checked for interval
  -- notwithstanding there isn't previous request,
  -- this is okay because it solves possible case when
  -- this service is restarted quickly just after previous request
  -- before restart and interval could be smaller than required.
  -- This initial state contains request counter and time of last response.
  getCurrentTime <&> ((0 :: Integer),)


handleRequest
  :: ( MonadReader AppContext m
     , S.MonadState (Integer, Time.UTCTime) m
     , LoggerBusMonad m
     , TimeMonad m
     , DelayMonad m
     , MVarMonad m
     , IORefWithCounterMonad m
     , ServantClientMonad m
     )
  => Float -> m ()
handleRequest nominatimReqGapInSeconds = do
  -- Waiting for next request
  requestArgs@(reqParams, _, responseBus) <-
    asks requestExecutorBus >>= takeMVar

  -- Increasing request counter
  S.modify' $ \(a, b) -> (a + 1, b)

  withCounter $ \n ->
    logInfo [qm| Executing request #{n} with params: {reqParams}... |]

  -- Checking if there's cached response by this request params
  cachedResponse <-
    M.lookup reqParams <$>
      (asks responsesCache >>= readIORefWithCounter)

  case cachedResponse of
       -- Nothing found in cache for this request, requesting it
       Nothing -> realRequest nominatimReqGapInSeconds requestArgs

       -- Found cached result, using it to return to user immediately
       Just (_, x) -> do
         withCounter $ \n ->
           logInfo [qms| Response for request #{n}
                         by params {reqParams} is taken from cache. |]

         putMVar responseBus $ Right x


-- Real request, when nothing found in cache.
-- Successful request will be added to cache.
realRequest
  :: ( MonadReader AppContext m
     , S.MonadState (Integer, Time.UTCTime) m
     , LoggerBusMonad m
     , TimeMonad m
     , DelayMonad m
     , MVarMonad m
     , IORefWithCounterMonad m
     , ServantClientMonad m
     )
  => Float
  -> (RequestParams, ClientM Response, MVar (Either ServantError Response))
  -> m ()
realRequest nominatimReqGapInSeconds (reqParams, req, responseBus) = do
  -- Handling minimal invervals between requests
  do lastTime <- S.gets snd
     curTime  <- getCurrentTime

     let timeDiffInMicroseconds = round $ secondInMicroseconds * x :: Int
           where
             x = fromRational
               $ toRational
               $ Time.diffUTCTime curTime lastTime

     when (timeDiffInMicroseconds < intervalBetweenRequests) $ do
       let waitTime = intervalBetweenRequests - timeDiffInMicroseconds
           waitTimeInSeconds = fromIntegral waitTime / secondInMicroseconds

       withCounter $ \n ->
         logInfo [qms| Request #{n} by params {reqParams}
                       is delayed by {waitTimeInSeconds} second(s)
                       to satisfy interval between requests
                       which is {intervalBetweenRequestsInSeconds}
                       second(s)... |]

       delay waitTime

  result <- asks clientEnv >>= runClientM req

  case result of
       Left e -> do
         withCounter $ \n ->
           logError [qms| Request #{n} by params {reqParams}
                          is failed with exception: {e}. |]

         getCurrentTime >>= S.modify' . fmap . const

       Right x -> do
         withCounter $ \n ->
           logInfo
             [qms| Request #{n} by params {reqParams} is succeeded,
                   adding result to the cache... |]

         utc <- getCurrentTime
         S.modify' $ fmap $ const utc

         asks responsesCache >>=
           flip modifyIORefWithCounter' (M.insert reqParams (utc, x))

  -- Sending response back
  putMVar responseBus result

  where
    -- Minimum interval is one second, making it little more safe
    intervalBetweenRequests =
      round $ secondInMicroseconds * nominatimReqGapInSeconds :: Int

    intervalBetweenRequestsInSeconds =
      fromIntegral intervalBetweenRequests / secondInMicroseconds :: Float


withCounter :: S.MonadState (Integer, a) m => (Integer -> m ()) -> m ()
withCounter = (S.gets fst >>=)
