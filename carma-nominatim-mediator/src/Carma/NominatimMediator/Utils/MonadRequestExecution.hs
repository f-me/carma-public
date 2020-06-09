{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Carma.NominatimMediator.Utils.MonadRequestExecution where

import           Control.Monad.Reader.Class (MonadReader, asks)
import           Control.Monad.Catch (MonadThrow, throwM, toException)

import           Servant.Client hiding (Response)

import           Carma.NominatimMediator.Types
import           Carma.NominatimMediator.Utils
import           Carma.Monad


class Monad m => MonadRequestExecution m where
  -- Sends request to requests executor bus
  -- and creates response bus and waits for response.
  executeRequest
    :: RequestParams
    -> ClientM Response
    -> m (StatisticResolve, Response)


instance ( Monad m
         , MonadReader AppContext m
         , MonadThrow m
         , MonadThread m
         , MonadDelay m
         , MonadMVar m
         ) => MonadRequestExecution m
         where

  executeRequest reqParams reqMonad = do
    responseBus <- newEmptyMVar

    -- Handling timeout case
    thread <- fork $ do
      delay timeout
      putMVar responseBus $ Left $
        ConnectionError $ toException $ RequestTimeoutException reqParams

    asks requestExecutorBus >>=
      flip putMVar (reqParams, reqMonad, responseBus)

    result <- takeMVar responseBus
    killThread thread -- No need for timeout thread anymore

    case result of
         Left  e -> throwM e
         Right x -> pure x

    where timeout = round $ 15 * secondInMicroseconds :: Int
