{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Carma.EraGlonass.Persistent where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.Trans.Control (MonadBaseControl)

import qualified Database.Persist.Sql as DB

import           Carma.Monad.PersistentSql
import           Carma.EraGlonass.Types
                   ( AppContext (dbConnection)
                   , DBConnection (..)
                   )


instance ( Monad m
         , MonadIO m
         , MonadBaseControl IO m
         , MonadReader AppContext m
         ) => MonadPersistentSql m
         where

  runSql m =
    asks dbConnection >>= \case
      DBConnection     x -> DB.runSqlConn m x
      DBConnectionPool x -> DB.runSqlPool m x

  -- | TODO Look at Database.Persist.Sql.Run.withResourceTimeout
  runSqlTimeout = error "Not implemented yet"



  -- @PersistStoreRead@

  get = DB.get

  -- Additional for @PersistStoreRead@ but not part of it

  getJust       = DB.getJust
  getJustEntity = DB.getJustEntity
  getEntity     = DB.getEntity
  belongsTo     = DB.belongsTo
  belongsToJust = DB.belongsToJust



  -- @PersistStoreWrite@

  insert           = DB.insert
  insert_          = DB.insert_
  insertMany       = DB.insertMany
  insertMany_      = DB.insertMany_
  insertEntityMany = DB.insertEntityMany
  insertKey        = DB.insertKey

  repsert = DB.repsert
  replace = DB.replace

  delete = DB.delete

  update    = DB.update
  updateGet = DB.updateGet

  -- Additional for @PersistStoreWrite@ but not part of it

  insertEntity = DB.insertEntity
  insertRecord = DB.insertRecord



  -- @PersistUniqueRead@

  getBy = DB.getBy

  -- Additional for @PersistUniqueRead@ but not part of it

  getByValue  = DB.getByValue
  checkUnique = DB.checkUnique



  -- @PersistUniqueWrite@

  deleteBy     = DB.deleteBy
  insertUnique = DB.insertUnique
  upsert       = DB.upsert
  upsertBy     = DB.upsertBy

  -- Additional for @PersistUniqueWrite@ but not part of it

  insertBy           = DB.insertBy
  insertUniqueEntity = DB.insertUniqueEntity
  replaceUnique      = DB.replaceUnique
  onlyUnique         = DB.onlyUnique



  -- @PersistQueryRead@

  -- selectSourceRes = DB.selectSourceRes
  selectFirst = DB.selectFirst
  -- selectKeysRes = DB.selectKeysRes
  count = DB.count

  -- Additional for @PersistQueryRead@ but not part of it

  -- selectSource = DB.selectSource
  -- selectKeys = DB.selectKeys
  selectList = DB.selectList
  selectKeysList = DB.selectKeysList



  -- @PersistQueryWrite@

  updateWhere = DB.updateWhere
  deleteWhere = DB.deleteWhere
