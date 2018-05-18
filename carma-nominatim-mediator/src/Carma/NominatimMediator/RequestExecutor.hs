{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Carma.NominatimMediator.RequestExecutor where

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
    (reqParams, req, responseBus) <-
      liftIO $ takeMVar $ requestExecutorBus appCtx

    logInfo appCtx [qm| Executing request with params: {reqParams}... |]
    result <- liftIO $ runClientM req $ clientEnv appCtx

    case result of
         Left e -> logError appCtx [qms| Request by params {reqParams}
                                         is failed with exception: {e}. |]

         Right _ -> logInfo appCtx [qms| Request by params {reqParams}
                                         is succeeded. |]

    liftIO $ putMVar responseBus result

    logInfo appCtx [qms| Request executor will wait for
                         {intervalBetweenRequestsInSeconds} seconds
                         before handling next request... |]

    liftIO $ threadDelay intervalBetweenRequests

  where
    -- Minimum interval is one second, making it little more safe
    intervalBetweenRequests = round $ secondInMicroseconds * 1.5

    intervalBetweenRequestsInSeconds =
      fromIntegral intervalBetweenRequests / secondInMicroseconds
