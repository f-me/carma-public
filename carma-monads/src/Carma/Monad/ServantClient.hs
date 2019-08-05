{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Carma.Monad.ServantClient
     ( MonadServantClient (..)
     , ServantError
     , ClientEnv
     , ClientM
     ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Servant.Client (runClientM)
import           Servant.Client (ServantError, ClientM, ClientEnv)


class Monad m => MonadServantClient m where
  runClientM :: ClientM a -> ClientEnv -> m (Either ServantError a)


instance (Monad m, MonadIO m) => MonadServantClient m where
  runClientM x = liftIO . Servant.Client.runClientM x
