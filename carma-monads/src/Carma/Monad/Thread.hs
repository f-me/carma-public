{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Carma.Monad.Thread
     ( MonadThread
     , SomeException
     , forkWithWaitBus
     , forkWithSem
     , MonadSTM
     , atomically
     , MonadMVar
     , MonadDelay
     , delay
     ) where

import           Control.Exception (SomeException)
import           Control.Monad.Base (MonadBase)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Monad.STM as STM
import           Control.Concurrent.Lifted
import           Control.Concurrent.STM.TSem


type MonadThread m = (Monad m, MonadBaseControl IO m)

-- | Forks and returns @MVar@ which will be notified when thread is done.
--
-- Known issues:
--   * If an exception is thrown via @Control.Monad.Error.Class.MonadError@
--     inside provided monad final notification of @waitBus@ won't happen since
--     that notification is combined by @(>>=)@ with provided monad
--     (be careful, catch such exceptions!).
forkWithWaitBus
  :: (MonadThread m, MonadMVar m)
  => m ()
  -> m (ThreadId, MVar ())
forkWithWaitBus m = do
  waitBus <- newEmptyMVar
  (,waitBus) <$> forkFinally m (\_ -> putMVar waitBus ())


-- | Forks and returns @TSem@ which will be notified when thread is done.
--
-- Known issues:
--   * If an exception is thrown via @Control.Monad.Error.Class.MonadError@
--     inside provided monad final notification of @waitSem@ won't happen since
--     that notification is combined by @(>>=)@ with provided monad
--     (be careful, catch such exceptions!).
forkWithSem :: (MonadThread m, MonadSTM m) => m () -> m (ThreadId, TSem)
forkWithSem m = do
  waitSem <- atomically $ newTSem 0
  (,waitSem) <$> forkFinally m (\_ -> atomically $ signalTSem waitSem)


type MonadSTM m = (Monad m, MonadIO m)

atomically :: MonadSTM m => STM.STM a -> m a
atomically = liftIO . STM.atomically

type MonadMVar m = (Monad m, MonadBase IO m)

type MonadDelay m = (Monad m, MonadBase IO m)

delay :: MonadDelay m => Int -> m ()
delay = threadDelay
