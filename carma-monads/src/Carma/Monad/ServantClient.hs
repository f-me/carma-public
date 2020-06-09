{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Carma.Monad.ServantClient
     ( MonadServantClient (..)
     , ClientError
     , ClientEnv
     , ClientM
     ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Servant.Client (runClientM)
import           Servant.Client (ClientError, ClientM, ClientEnv)


class Monad m => MonadServantClient m where
  runClientM :: ClientM a -> ClientEnv -> m (Either ClientError a)


instance (Monad m, MonadIO m) => MonadServantClient m where
  runClientM x = liftIO . Servant.Client.runClientM x
