{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Carma.Monad.STM
     ( MonadSTM (..)
     , STM.STM
     ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.STM as STM


class Monad m => MonadSTM m where
  atomically :: STM.STM a -> m a


instance (Monad m, MonadIO m) => MonadSTM m where
  atomically = liftIO . STM.atomically
