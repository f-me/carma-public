{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Carma.Monad.ServantClient
     ( MonadServantClient
     , runClientM
     , ClientError
     , ClientEnv
     , ClientM
     ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Servant.Client (runClientM)
import           Servant.Client (ClientError, ClientM, ClientEnv)


type MonadServantClient m = (Monad m, MonadIO m)

runClientM :: MonadServantClient m
           => ClientM a -> ClientEnv -> m (Either ClientError a)
runClientM x = liftIO . Servant.Client.runClientM x
