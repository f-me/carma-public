{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Carma.Monad.Thread
     ( MonadThread (..)
     , Lifted.ThreadId
     , SomeException
     , forkWithWaitBus
     , forkWithSem
     ) where

import           Control.Exception (SomeException)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Concurrent.Lifted as Lifted
import           Control.Concurrent.STM.TSem

import           Carma.Monad.STM
import           Carma.Monad.MVar


class Monad m => MonadThread m where
  fork        :: m () -> m Lifted.ThreadId
  forkFinally :: m a -> (Either SomeException a -> m ()) -> m Lifted.ThreadId
  killThread  :: Lifted.ThreadId -> m ()


instance (Monad m, MonadBaseControl IO m) => MonadThread m where
  fork        = Lifted.fork
  forkFinally = Lifted.forkFinally
  killThread  = Lifted.killThread


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
  -> m (Lifted.ThreadId, Lifted.MVar ())
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
forkWithSem :: (MonadThread m, MonadSTM m) => m () -> m (Lifted.ThreadId, TSem)
forkWithSem m = do
  waitSem <- atomically $ newTSem 0
  (,waitSem) <$> forkFinally m (\_ -> atomically $ signalTSem waitSem)
