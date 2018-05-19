{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Carma.NominatimMediator.RequestExecutor where

import           Data.IORef
import qualified Data.Map as M
import qualified Data.Time.Clock as Time
import           Text.InterpolatedString.QM

import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Concurrent

import           Servant.Client

import           Carma.NominatimMediator.Types
import           Carma.NominatimMediator.Utils
import           Carma.NominatimMediator.Logger


-- Requests queue.
-- Supposed to be run in own thread.
-- It writes response to provided `MVar`.
requestExecutorInit :: (LoggerBus m, MonadIO m) => AppContext -> m ()
requestExecutorInit appCtx = do
  logInfo appCtx [qms| Running request executor,
                       waiting for {intervalBetweenRequestsInSeconds} seconds
                       before start in case application is restarted quickly
                       just after previous request... |]

  liftIO $ threadDelay intervalBetweenRequests
  logInfo appCtx [qn| Request executor is ready and waiting for requests... |]

  forever $ do
    requestArgs@(reqParams, _, responseBus) <-
      liftIO $ takeMVar $ requestExecutorBus appCtx

    logInfo appCtx [qm| Executing request with params: {reqParams}... |]

    cachedResponse <- liftIO $
      fmap (M.lookup reqParams) $ readIORef $ responsesCache appCtx

    case cachedResponse of
         -- Nothing found in cache for this request, requesting it
         Nothing -> request requestArgs

         -- Found cached result, using it to return to user immediately
         Just (_, x) -> do
           logInfo appCtx [qms| Response for request by params {reqParams}
                                is taken from cache. |]

           liftIO $ putMVar responseBus $ Right x

  where
    -- Minimum interval is one second, making it little more safe
    intervalBetweenRequests = round $ secondInMicroseconds * 1.5

    intervalBetweenRequestsInSeconds =
      fromIntegral intervalBetweenRequests / secondInMicroseconds

    -- Real request, when nothing found in cache.
    -- Successful request will be added to cache.
    request (reqParams, req, responseBus) = do
      result <- liftIO $ runClientM req $ clientEnv appCtx

      case result of
           Left e ->
             logError appCtx [qms| Request by params {reqParams}
                                   is failed with exception: {e}. |]

           Right x -> do
             logInfo appCtx [qms| Request by params {reqParams} is succeeded,
                                  adding result to the cache... |]

             utc <- liftIO Time.getCurrentTime

             liftIO $
               responsesCache appCtx `modifyIORef'` M.insert reqParams (utc, x)

      liftIO $ putMVar responseBus result

      logInfo appCtx [qms| Request executor will wait for
                           {intervalBetweenRequestsInSeconds} seconds
                           before handling next request... |]

      -- TODO No need to wait in next response would be taken from cache,
      --      it would be a good idea to calculate time delta
      --      and wait only when it makes sense.
      --      I think I would use StateT monad for `forever` to update last
      --      request finish time (and also served requests counter).
      liftIO $ threadDelay intervalBetweenRequests
