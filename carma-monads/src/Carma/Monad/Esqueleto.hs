{-# LANGUAGE RankNTypes, GADTs, FlexibleInstances, UndecidableInstances #-}

module Carma.Monad.Esqueleto
     ( MonadEsqueleto (..)
     ) where

import           Data.Int (Int64)

import           Control.Monad.IO.Class (MonadIO)

import qualified Database.Esqueleto as E
import           Database.Esqueleto.Internal.Sql (SqlSelect)
import           Database.Esqueleto.Internal.Language (Insertion)

import           Database.Esqueleto
                   ( SqlQuery
                   , SqlExpr
                   , SqlReadT
                   , SqlWriteT
                   , Entity
                   , SqlEntity
                   , PersistEntity
                   )

import           Carma.Monad.PersistentSql (MonadPersistentSql)


class MonadPersistentSql m => MonadEsqueleto m where
  esqueletoSelect :: SqlSelect a r => SqlQuery a -> SqlReadT m [r]

  esqueletoDelete :: SqlQuery () -> SqlWriteT m ()
  esqueletoDeleteCount :: SqlQuery () -> SqlWriteT m Int64

  esqueletoUpdate
    :: SqlEntity val
    => (SqlExpr (Entity val) -> SqlQuery ())
    -> SqlWriteT m ()

  esqueletoUpdateCount
    :: SqlEntity val
    => (SqlExpr (Entity val) -> SqlQuery ())
    -> SqlWriteT m Int64

  esqueletoInsertSelect
    :: PersistEntity a
    => SqlQuery (SqlExpr (Insertion a))
    -> SqlWriteT m ()

  esqueletoInsertSelectCount
    :: PersistEntity a
    => SqlQuery (SqlExpr (Insertion a))
    -> SqlWriteT m Int64


instance ( Monad m
         , MonadIO m
         , MonadPersistentSql m
         ) => MonadEsqueleto m
         where

  esqueletoSelect            = E.select

  esqueletoDelete            = E.delete
  esqueletoDeleteCount       = E.deleteCount

  esqueletoUpdate            = E.update
  esqueletoUpdateCount       = E.updateCount

  esqueletoInsertSelect      = E.insertSelect
  esqueletoInsertSelectCount = E.insertSelectCount
