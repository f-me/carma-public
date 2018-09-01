{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Carma.Monad.Clock
     ( MonadClock (..)
     , Clock.UTCTime
     ) where

import qualified Data.Time.Clock as Clock

import           Control.Monad.IO.Class (MonadIO, liftIO)


class Monad m => MonadClock m where
  getCurrentTime :: m Clock.UTCTime


instance (Monad m, MonadIO m) => MonadClock m where
  getCurrentTime = liftIO Clock.getCurrentTime
