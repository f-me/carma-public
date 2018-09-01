{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Carma.Monad.Thread
     ( MonadThread (..)
     , Lifted.ThreadId
     , SomeException
     , forkWithWaitBus
     ) where

import           Control.Exception (SomeException)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Concurrent.Lifted as Lifted

import           Carma.Monad.MVar


class Monad m => MonadThread m where
  fork        :: m () -> m Lifted.ThreadId
  forkFinally :: m a -> (Either SomeException a -> m ()) -> m Lifted.ThreadId
  killThread  :: Lifted.ThreadId -> m ()


instance (Monad m, MonadBaseControl IO m) => MonadThread m where
  fork        = Lifted.fork
  forkFinally = Lifted.forkFinally
  killThread  = Lifted.killThread


-- Forks and returns `MVar` which will be notified when thread is done.
forkWithWaitBus
  :: (MonadThread m, MonadMVar m)
  => m () -> m (Lifted.ThreadId, Lifted.MVar ())
forkWithWaitBus m = do
  waitBus <- newEmptyMVar
  (,waitBus) <$> forkFinally m (\_ -> putMVar waitBus ())
