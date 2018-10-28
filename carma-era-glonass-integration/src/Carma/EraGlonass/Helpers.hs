module Carma.EraGlonass.Helpers
     ( runSqlTransaction
     , runSqlTransactionTimeout
     ) where

import           Control.Monad.Reader (ReaderT)
import           Control.Exception (SomeException)

import           Database.Persist.Sql (SqlBackend)

import           Carma.Monad.PersistentSql (MonadPersistentSql (..))


runSqlTransaction
  :: MonadPersistentSql m
  => ReaderT SqlBackend m a
  -> m a

runSqlTransaction m =
  runSql $ transactionSave >> m >>= (<$ transactionSave)


runSqlTransactionTimeout
  :: MonadPersistentSql m
  => Int
  -> ReaderT SqlBackend m a
  -> m (Either SomeException a)

runSqlTransactionTimeout n m =
  runSqlTimeout n $ transactionSave >> m >>= (<$ transactionSave)
