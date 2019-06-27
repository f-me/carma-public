{-# LANGUAGE OverloadedStrings, ConstraintKinds, FlexibleContexts #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}

module Carma.EraGlonass.Server.ReceiveRequestForServiceRequest
     ( receiveRequestForServiceRequest
     ) where

import           Control.Monad.Reader (MonadReader)
import           Control.Monad.Error.Class (MonadError, throwError)
import           Control.Monad.Random.Class (MonadRandom)

import           Servant

import           Carma.Monad.STM
import           Carma.Monad.MVar
import           Carma.Monad.Clock
import           Carma.Monad.Thread
import           Carma.Monad.LoggerBus
import           Carma.Monad.PersistentSql
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Types.AppContext (AppContext)
import           Carma.EraGlonass.Types.EGRequestForServiceRequest


-- | Receive request for service monads constraint.
type ReceiveRequestForServiceRequestMonad m =
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadError ServantErr m
   , MonadPersistentSql m
   , MonadClock m
   , MonadRandom m
   , MonadThread m
   , MonadMVar m
   , MonadSTM m
   )


-- | TODO implement
receiveRequestForServiceRequest
  :: ReceiveRequestForServiceRequestMonad m
  => EGRequestForServiceRequest
  -> m ()

receiveRequestForServiceRequest _ =
  throwError err500
    { errBody = "TODO Implement ReceiveRequestForServiceRequest" }
