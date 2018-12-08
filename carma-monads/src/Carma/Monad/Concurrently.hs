{-# LANGUAGE UndecidableInstances, FlexibleInstances, FlexibleContexts #-}

module Carma.Monad.Concurrently
     ( MonadConcurrently (..)
     , Concurrently (..)

     , mapConcurrently
     , mapConcurrently_

     , forConcurrently
     , forConcurrently_

     , replicateConcurrently
     , replicateConcurrently_
     ) where

import           Data.Semigroup
import           Data.Foldable (fold)

import           Control.Monad
import           Control.Applicative
import           Control.Monad.Trans.Control (MonadBaseControl)

import qualified Control.Concurrent.Async.Lifted as C


-- | This monad could be extended with
class Monad m => MonadConcurrently m where
  runConcurrently :: Concurrently m a -> m a

  -- | For internal usage (to build "Applicative" instance)
  concurrentlyApplicate
    :: Concurrently m (a -> b) -> Concurrently m a -> Concurrently m b

  -- | For internal usage (to build "Alternative" instance)
  concurrentlyAlternate
    :: Concurrently m a -> Concurrently m a -> Concurrently m a

  -- | For internal usage (to build "Alternative" instance)
  concurrentlyAlternateEmpty :: Concurrently m a

-- | This is just a mirror of __lifted-async__ package.
instance (Monad m, MonadBaseControl IO m) => MonadConcurrently m where
  -- | It's not really a runner, but a wrapped monad already has concurrent
  -- logic (produced by using "Applicative" and "Alternative" instances).
  runConcurrently (Concurrently m) = m

  concurrentlyApplicate (Concurrently a) (Concurrently b) =
    case C.Concurrently a <*> C.Concurrently b of
         C.Concurrently m -> Concurrently m

  concurrentlyAlternate (Concurrently a) (Concurrently b) =
    case C.Concurrently a <|> C.Concurrently b of
         C.Concurrently m -> Concurrently m

  -- | In __async__ package it's defined as @threadDelay@ of @maxBound@,
  -- so I guess it's just a plug and you aren't supposed to use this.
  concurrentlyAlternateEmpty = case empty of C.Concurrently m -> Concurrently m


newtype Concurrently m a = Concurrently (m a)

instance (Monad m, MonadConcurrently m, Functor m)
      => Functor (Concurrently m) where

  fmap f (Concurrently m) = Concurrently $ f <$> m

instance (Monad m, MonadConcurrently m) => Applicative (Concurrently m) where
  pure = Concurrently . pure
  (<*>) = concurrentlyApplicate

instance (Monad m, MonadConcurrently m) => Alternative (Concurrently m) where
  empty = concurrentlyAlternateEmpty
  (<|>) = concurrentlyAlternate

instance (Monad m, MonadConcurrently m, Semigroup a)
      => Semigroup (Concurrently m a) where

  (<>) = liftA2 (<>)

instance (Monad m, MonadConcurrently m, Semigroup a, Monoid a)
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
