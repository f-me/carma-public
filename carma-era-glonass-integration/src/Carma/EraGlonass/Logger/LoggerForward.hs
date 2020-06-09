{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | This is just a clone of the @ReaderT@ but with fixed "environment".
--
-- The purpose of this is to resolve overlapping instances issue.
module Carma.EraGlonass.Logger.LoggerForward
     ( LoggerForward (runLoggerForward)
     , askLoggerForward
     ) where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Base (MonadBase (liftBase), liftBaseDefault)

import           Control.Monad.Trans.Control
                   ( MonadBaseControl (StM, liftBaseWith, restoreM)
                   , MonadTransControl (StT, liftWith, restoreT)
                   , ComposeSt
                   , defaultLiftBaseWith
                   , defaultRestoreM
                   )

import           Carma.Monad.LoggerBus.Types (LogMessage)
import           Control.Concurrent.STM.TQueue


newtype LoggerForward m a
      = LoggerForward { runLoggerForward :: TQueue LogMessage -> m a }

liftLoggerForward :: m a -> LoggerForward m a
liftLoggerForward = LoggerForward . const
{-# INLINE liftLoggerForward #-}

mapLoggerForward :: (m a -> n b) -> LoggerForward m a -> LoggerForward n b
mapLoggerForward f m = LoggerForward $ f . runLoggerForward m
{-# INLINE mapLoggerForward #-}

askLoggerForward :: Monad m => LoggerForward m (TQueue LogMessage)
askLoggerForward = LoggerForward return
{-# INLINE askLoggerForward #-}

instance Functor m => Functor (LoggerForward m) where
  fmap = mapLoggerForward . fmap
  {-# INLINE fmap #-}

instance Applicative m => Applicative (LoggerForward m) where
  pure = liftLoggerForward . pure
  {-# INLINE pure #-}
  f <*> v
    = LoggerForward
    $ \mVar -> runLoggerForward f mVar <*> runLoggerForward v mVar
  {-# INLINE (<*>) #-}

instance Monad m => Monad (LoggerForward m) where
  return = lift . return
  {-# INLINE return #-}

  m >>= k =
    LoggerForward $ \mVar -> do
      a <- runLoggerForward m mVar
      runLoggerForward (k a) mVar
  {-# INLINE (>>=) #-}

instance MonadFail m => MonadFail (LoggerForward m) where
  fail = lift . fail
  {-# INLINE fail #-}

instance MonadTrans LoggerForward where
  lift = liftLoggerForward
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (LoggerForward m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance MonadBase b m => MonadBase b (LoggerForward m) where
  liftBase = liftBaseDefault
  {-# INLINE liftBase #-}

instance MonadBaseControl b m => MonadBaseControl b (LoggerForward m) where
  type StM (LoggerForward m) a = ComposeSt LoggerForward m a
  liftBaseWith = defaultLiftBaseWith
  {-# INLINABLE liftBaseWith #-}
  restoreM = defaultRestoreM
  {-# INLINABLE restoreM #-}

instance MonadTransControl LoggerForward where
  type StT LoggerForward a = a
  liftWith f = LoggerForward $ \r -> f $ \t -> runLoggerForward t r
  {-# INLINABLE liftWith #-}
  restoreT = LoggerForward . const
  {-# INLINABLE restoreT #-}
