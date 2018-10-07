{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}
{-# LANGUAGE QuasiQuotes #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | VIN synchronizer worker module.
module Carma.EraGlonass.VinSynchronizer
     ( runVinSynchronizer
     ) where

import           Text.InterpolatedString.QM

import           Control.Monad.Reader (MonadReader)

import           Carma.Monad
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Types


-- | VIN synchronizer monad constraint.
type VinSynchronizerMonad m =
   ( MonadReader AppContext m
   , MonadLoggerBus m
   )


-- | VIN synchronizer worker starter.
runVinSynchronizer :: VinSynchronizerMonad m => m ()
runVinSynchronizer = do
  logInfo [qm| Running VIN synchronizer worker (TODO)... |]
