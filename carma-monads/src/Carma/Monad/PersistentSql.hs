module Carma.Monad.PersistentSql
     ( MonadPersistentSql (..)
     ) where

import           Control.Monad.Reader (ReaderT)

import           Database.Persist.Sql


class Monad m => MonadPersistentSql m where
  runSql :: ReaderT SqlBackend m a -> m a
  -- runSqlTimeout :: Int -> ReaderT SqlBackend m a -> m (Maybe a)
