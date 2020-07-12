{-# LANGUAGE UndecidableInstances, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Carma.Monad.Concurrently
     ( MonadConcurrently
     , Concurrently (..)

     , mapConcurrently
     , mapConcurrently_

     , forConcurrently
     , forConcurrently_

     , replicateConcurrently
     , replicateConcurrently_
     ) where

import           Data.Foldable (fold)

import           Control.Monad
import           Control.Applicative
import           Control.Monad.Trans.Control (MonadBaseControl)

import qualified Control.Concurrent.Async.Lifted as C


newtype Concurrently m a = Concurrently { runConcurrently :: m a }
type MonadConcurrently m = (Monad m, MonadBaseControl IO m)

instance MonadConcurrently m => Functor (Concurrently m) where
  fmap f (Concurrently m) = Concurrently $ f <$> m

instance MonadConcurrently m => Applicative (Concurrently m) where
  pure = Concurrently . pure

  -- | "<*>" stands for \"parallel"
  Concurrently a <*> Concurrently b =
    case C.Concurrently a <*> C.Concurrently b of
         C.Concurrently m -> Concurrently m

instance MonadConcurrently m => Alternative (Concurrently m) where
  -- | In __async__ package it's defined as @threadDelay@ of @maxBound@,
  -- so I guess it's just a plug and you aren't supposed to use this.
  empty = case empty of C.Concurrently m -> Concurrently m

  -- | "<|>" stands for \"race" with the same type of result of both actions
  Concurrently a <|> Concurrently b =
    case C.Concurrently a <|> C.Concurrently b of
         C.Concurrently m -> Concurrently m

instance (MonadConcurrently m, Semigroup a)
      => Semigroup (Concurrently m a) where
  (<>) = liftA2 (<>)

instance (MonadConcurrently m, Semigroup a, Monoid a)
      => Monoid (Concurrently m a) where
  mempty = pure mempty
  mappend = (<>)


mapConcurrently
  :: (Traversable t, MonadConcurrently m)
  => (a -> m b)
  -> t a
  -> m (t b)
mapConcurrently f = runConcurrently . traverse (Concurrently . f)

mapConcurrently_
  :: (Foldable t, MonadConcurrently m)
  => (a -> m b)
  -> t a
  -> m ()
mapConcurrently_ f = runConcurrently . foldMap (Concurrently . void . f)


forConcurrently
  :: (Traversable t, MonadConcurrently m)
  => t a
  -> (a -> m b)
  -> m (t b)
forConcurrently = flip mapConcurrently

forConcurrently_
  :: (Foldable t, MonadConcurrently m)
  => t a
  -> (a -> m b)
  -> m ()
forConcurrently_ = flip mapConcurrently_


replicateConcurrently
  :: MonadConcurrently m
  => Int
  -> m a
  -> m [a]
replicateConcurrently n =
  runConcurrently . sequenceA . replicate n . Concurrently

replicateConcurrently_
  :: MonadConcurrently m
  => Int
  -> m a
  -> m ()
replicateConcurrently_ n =
  runConcurrently . fold . replicate n . Concurrently . void
