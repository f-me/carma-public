{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Carma.Monad.IORefWithCounter
     ( -- Constructor isn't exported.
       -- Only `MonadIORefWithCounter` allowed for manipulations
       -- to prevent human-factor mistakes.
       -- If you missed something for `IORef` please just implement it in
       -- `MonadIORefWithCounter` and do not forget to update a counter if you
       -- modify something.
       IORefWithCounter
     , MonadIORefWithCounter (..)
     ) where

import           Data.IORef

import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Carma.Utils


newtype IORefWithCounter a = IORefWithCounter (IORef (Integer, a))


class Monad m => MonadIORefWithCounter m where
  newIORefWithCounter           :: a -> m (IORefWithCounter a)

  {-# INLINE readIORefWithCounter #-}
  readIORefWithCounter          :: IORefWithCounter a -> m a
  readIORefWithCounter x        = readIORefWithCounter' x <&!> snd

  readIORefWithCounter'         :: IORefWithCounter a -> m (Integer, a)

  -- `Eq` is needed to check if something changed to increment the counter
  modifyIORefWithCounter'       :: Eq a
                                => IORefWithCounter a -> (a -> a) -> m ()

  -- `Eq` is needed to check if something changed to increment the counter
  atomicModifyIORefWithCounter' :: Eq a
                                => IORefWithCounter a -> (a -> (a, b)) -> m b


instance (Monad m, MonadIO m) => MonadIORefWithCounter m where
  newIORefWithCounter x = liftIO $ newIORef (0, x) <&!> IORefWithCounter
  readIORefWithCounter' (IORefWithCounter x) = liftIO $ readIORef x

  modifyIORefWithCounter' (IORefWithCounter x) f =
    liftIO $ x `modifyIORef'`
      \(c, v) -> let v' = f v in (if v' /= v then succ c else c, v')

  atomicModifyIORefWithCounter' (IORefWithCounter x) f =
    liftIO $ x `atomicModifyIORef'`
      \(c, v) -> let (v', a) = f v in ((if v' /= v then succ c else c, v'), a)
