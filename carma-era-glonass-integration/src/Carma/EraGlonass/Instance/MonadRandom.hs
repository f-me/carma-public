{-# OPTIONS_GHC -fno-warn-orphans #-}

module Carma.EraGlonass.Instance.MonadRandom () where

import           Control.Monad.Random.Class
import           Control.Monad.IO.Class (liftIO)

import           Servant


instance MonadRandom Handler where
  getRandomR  = liftIO . getRandomR
  getRandom   = liftIO   getRandom
  getRandomRs = liftIO . getRandomRs
  getRandoms  = liftIO   getRandoms
