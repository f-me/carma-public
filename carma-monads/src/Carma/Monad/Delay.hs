{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Carma.Monad.Delay
     ( MonadDelay (..)
     ) where

import           Control.Monad.Base (MonadBase)
import qualified Control.Concurrent.Lifted as Lifted


class Monad m => MonadDelay m where
  delay :: Int -> m ()


instance (Monad m, MonadBase IO m) => MonadDelay m where
  delay = Lifted.threadDelay
