{-# LANGUAGE GADTs #-}

module Carma.Monad.PersistentSql
     ( MonadPersistentSql (..)
     ) where

import           Control.Monad.Reader (ReaderT)

import           Database.Persist.Sql


-- | If you miss any DB action just add it here and make an alias in
-- implementations.
class Monad m => MonadPersistentSql m where

  runSql :: ReaderT SqlBackend m a -> m a
  runSqlTimeout :: Int -> ReaderT SqlBackend m a -> m (Maybe a)

  -- | An example of implementation for @MonadIO@:
  -- @insert = Database.Persist.Sql.insert@
  insert
    :: PersistRecordBackend record SqlBackend
    => record
    -> ReaderT SqlBackend m (Key record)
