{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Carma.Monad.MVar
     ( MonadMVar (..)
     , Lifted.MVar
     ) where

import           Control.Monad.Base (MonadBase)
import qualified Control.Concurrent.Lifted as Lifted


class Monad m => MonadMVar m where
  newEmptyMVar :: m (Lifted.MVar a)
  newMVar      :: a -> m (Lifted.MVar a)
  putMVar      :: Lifted.MVar a -> a -> m ()
  takeMVar     :: Lifted.MVar a -> m a


instance (Monad m, MonadBase IO m) => MonadMVar m where
  newEmptyMVar = Lifted.newEmptyMVar
  newMVar      = Lifted.newMVar
  putMVar      = Lifted.putMVar
  takeMVar     = Lifted.takeMVar
