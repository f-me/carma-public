-- This module has some miscellaneous functions/values used in this service.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Carma.NominatimMediator.Utils
     ( (?), (<&>), (<&!>)

     , secondInMicroseconds
     , timeFormat
     , formatTime
     , floatShow
     , unwrapperToProxy

       -- Constructor isn't exported.
       -- Only `IORefWithCounterMonad` allowed for manipulations
       -- to prevent human-factor mistakes.
       -- If you missed something for `IORef` please just implement it in
       -- `IORefWithCounterMonad` and do not forget to update a counter if you
       -- modify something.
     , IORefWithCounter
     , IORefWithCounterMonad (..)

       -- Moands for side-effects abstractions
     , ThreadMonad (..)
     , forkWithWaitBus
     , DelayMonad (..)
     , MVarMonad (..)
     , TimeMonad (..)
     , FileMonad (..)
     , ServantClientMonad (..)
     ) where

import           Prelude hiding (readFile, writeFile)
import qualified Prelude

import           Data.Proxy
import           Data.IORef
import qualified Data.Time.Format as Time
import qualified Data.Time.Clock as Time
import           Text.Printf (printf)

import           Control.Arrow
import           Control.Exception (SomeException)
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Base (MonadBase)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Concurrent.Lifted as Lifted

import qualified System.Directory (doesFileExist)

import qualified Servant.Client (runClientM)
import           Servant.Client (ServantError, ClientM, ClientEnv)


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

floatShow :: Float -> String
floatShow = printf "%f"

unwrapperToProxy :: (a -> b) -> Proxy b
unwrapperToProxy _ = Proxy


newtype IORefWithCounter a = IORefWithCounter (IORef (Integer, a))

class Monad m => IORefWithCounterMonad m where
  newIORefWithCounter           :: a -> m (IORefWithCounter a)

  {-# INLINE readIORefWithCounter #-}
  readIORefWithCounter          :: IORefWithCounter a -> m a
  readIORefWithCounter x        = readIORefWithCounter' x <&!> snd

  readIORefWithCounter'         :: IORefWithCounter a -> m (Integer, a)

  modifyIORefWithCounter'       :: IORefWithCounter a -> (a -> a) -> m ()
  atomicModifyIORefWithCounter' :: IORefWithCounter a -> (a -> (a, b)) -> m b

instance (Monad m, MonadIO m) => IORefWithCounterMonad m where
  newIORefWithCounter x = liftIO $ newIORef (0, x) <&!> IORefWithCounter
  readIORefWithCounter' (IORefWithCounter x) = liftIO $ readIORef x

  modifyIORefWithCounter' (IORefWithCounter x) f =
    liftIO $ x `modifyIORef'` ((+ 1) *** f)

  atomicModifyIORefWithCounter' (IORefWithCounter x) f =
    liftIO $ x `atomicModifyIORef'`
      \(c, v) -> let (v', a) = f v in ((c + 1, v'), a)


-- Some monads to abstract side-effects

class Monad m => ThreadMonad m where
  fork        :: m () -> m Lifted.ThreadId
  forkFinally :: m a -> (Either SomeException a -> m ()) -> m Lifted.ThreadId
  killThread  :: Lifted.ThreadId -> m ()

instance (Monad m, MonadBaseControl IO m) => ThreadMonad m where
  fork        = Lifted.fork
  forkFinally = Lifted.forkFinally
  killThread  = Lifted.killThread

-- Forks and returns `MVar` which will be notified when thread is done.
forkWithWaitBus
  :: (ThreadMonad m, MVarMonad m)
  => m () -> m (Lifted.ThreadId, Lifted.MVar ())
forkWithWaitBus m = do
  waitBus <- newEmptyMVar
  (,waitBus) <$> forkFinally m (\_ -> putMVar waitBus ())

class Monad m => DelayMonad m where
  delay :: Int -> m ()

instance (Monad m, MonadBase IO m) => DelayMonad m where
  delay = Lifted.threadDelay

class Monad m => MVarMonad m where
  newEmptyMVar :: m (Lifted.MVar a)
  newMVar      :: a -> m (Lifted.MVar a)
  putMVar      :: Lifted.MVar a -> a -> m ()
  takeMVar     :: Lifted.MVar a -> m a

instance (Monad m, MonadBase IO m) => MVarMonad m where
  newEmptyMVar = Lifted.newEmptyMVar
  newMVar      = Lifted.newMVar
  putMVar      = Lifted.putMVar
  takeMVar     = Lifted.takeMVar

class Monad m => TimeMonad m where
  getCurrentTime :: m Time.UTCTime

instance (Monad m, MonadIO m) => TimeMonad m where
  getCurrentTime = liftIO Time.getCurrentTime

class Monad m => ServantClientMonad m where
  runClientM :: ClientM a -> ClientEnv -> m (Either ServantError a)

instance (Monad m, MonadIO m) => ServantClientMonad m where
  runClientM x = liftIO . Servant.Client.runClientM x

class Monad m => FileMonad m where
  readFile      :: FilePath -> m String
  writeFile     :: FilePath -> String -> m ()
  doesFileExist :: FilePath -> m Bool

instance (Monad m, MonadIO m) => FileMonad m where
  readFile      = liftIO . Prelude.readFile
  writeFile x   = liftIO . Prelude.writeFile x
  doesFileExist = liftIO . System.Directory.doesFileExist
