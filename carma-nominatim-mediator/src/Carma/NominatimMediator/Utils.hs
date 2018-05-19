-- This module has some miscellaneous functions/values used in this service.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}

module Carma.NominatimMediator.Utils
     ( (?), (<&>), (<&!>)

     , secondInMicroseconds
     , timeFormat
     , formatTime

       -- Constructor isn't exported.
       -- Only `IORefWithCounterM` allowed for manipulations
       -- to prevent human-factor mistakes.
       -- If you missed something for `IORef` please just implement it in
       -- `IORefWithCounterM` and do not forget to update a counter if you
       -- modify something.
     , IORefWithCounter
     , IORefWithCounterM (..)
     ) where

import           Data.IORef
import qualified Data.Time.Format as Time

import           Control.Arrow
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)


(?) :: (a -> b) -> (b -> c) -> (a -> c)
(?) = flip (.)
{-# INLINE (?) #-}
infixl 9 ?

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
{-# INLINE (<&>) #-}
infixr 5 <&>

(<&!>) :: Monad m => m a -> (a -> b) -> m b
(<&!>) = flip (<$!>)
{-# INLINE (<&!>) #-}
infixr 5 <&!>

secondInMicroseconds :: Float
secondInMicroseconds = 1000 * 1000

timeFormat :: String
timeFormat = "%Y-%m-%d %H:%M:%S"

formatTime :: Time.FormatTime t => t -> String
formatTime = Time.formatTime Time.defaultTimeLocale timeFormat


newtype IORefWithCounter a = IORefWithCounter (IORef (Integer, a))

class Monad m => IORefWithCounterM m where
  newIORefWithCounter           :: a -> m (IORefWithCounter a)

  {-# INLINE readIORefWithCounter #-}
  readIORefWithCounter          :: IORefWithCounter a -> m a
  readIORefWithCounter x        = readIORefWithCounter' x <&!> snd

  readIORefWithCounter'         :: IORefWithCounter a -> m (Integer, a)

  modifyIORefWithCounter'       :: IORefWithCounter a -> (a -> a) -> m ()
  atomicModifyIORefWithCounter' :: IORefWithCounter a -> (a -> (a, b)) -> m b

instance (Monad m, MonadIO m) => IORefWithCounterM m where
  newIORefWithCounter x = liftIO $ newIORef (0, x) <&!> IORefWithCounter
  readIORefWithCounter' (IORefWithCounter x) = liftIO $ readIORef x

  modifyIORefWithCounter' (IORefWithCounter x) f =
    liftIO $ x `modifyIORef'` ((+ 1) *** f)

  atomicModifyIORefWithCounter' (IORefWithCounter x) f =
    liftIO $ x `atomicModifyIORef'`
      \(c, v) -> let (v', a) = f v in ((c + 1, v'), a)
