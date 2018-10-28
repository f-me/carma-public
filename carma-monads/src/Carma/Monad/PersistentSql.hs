{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Carma.Monad.PersistentSql
     ( MonadPersistentSql (..)
     ) where

import           Control.Monad.Reader (ReaderT)
import           Control.Exception (SomeException)

import           Database.Persist.Sql


-- | If you miss any DB action just add it here and make an alias in
-- implementations.
--
-- See docs of @Database.Persist.Class@ for details.
class Monad m => MonadPersistentSql m where

  runSql :: ReaderT SqlBackend m a -> m a
  runSqlTimeout :: Int -> ReaderT SqlBackend m a -> m (Either SomeException a)

  transactionSave :: ReaderT SqlBackend m ()
  transactionUndo :: ReaderT SqlBackend m ()



  -- @PersistStoreRead@

  -- | An example of implementation for @MonadIO@:
  -- @get = Database.Persist.Sql.get@
  get
    :: PersistRecordBackend record SqlBackend
    => Key record
    -> ReaderT SqlBackend m (Maybe record)

  -- Additional for @PersistStoreRead@ but not part of it

  getJust
    :: ( Show (Key record)
       , PersistRecordBackend record SqlBackend
       )
    => Key record
    -> ReaderT SqlBackend m record
  getJustEntity
    :: ( PersistEntityBackend record ~ BaseBackend SqlBackend
       , PersistEntity record
       )
    => Key record
    -> ReaderT SqlBackend m (Entity record)
  getEntity
    :: PersistRecordBackend e SqlBackend
    => Key e
    -> ReaderT SqlBackend m (Maybe (Entity e))
  belongsTo
    :: ( PersistEntity ent1
       , PersistRecordBackend ent2 SqlBackend
       )
    => (ent1 -> Maybe (Key ent2))
    -> ent1
    -> ReaderT SqlBackend m (Maybe ent2)
  belongsToJust
    :: ( PersistEntity ent1
       , PersistRecordBackend ent2 SqlBackend
       )
    => (ent1 -> Key ent2)
    -> ent1
    -> ReaderT SqlBackend m ent2



  -- @PersistStoreWrite@

  insert
    :: PersistRecordBackend record SqlBackend
    => record
    -> ReaderT SqlBackend m (Key record)
  insert_
    :: PersistRecordBackend record SqlBackend
    => record
    -> ReaderT SqlBackend m ()
  insertMany
    :: PersistRecordBackend record SqlBackend
    => [record]
    -> ReaderT SqlBackend m [Key record]
  insertMany_
    :: PersistRecordBackend record SqlBackend
    => [record]
    -> ReaderT SqlBackend m ()
  insertEntityMany
    :: PersistRecordBackend record SqlBackend
    => [Entity record]
    -> ReaderT SqlBackend m ()
  insertKey
    :: PersistRecordBackend record SqlBackend
    => Key record
    -> record
    -> ReaderT SqlBackend m ()

  repsert
    :: PersistRecordBackend record SqlBackend
    => Key record
    -> record
    -> ReaderT SqlBackend m ()
  replace
    :: PersistRecordBackend record SqlBackend
    => Key record
    -> record
    -> ReaderT SqlBackend m ()

  delete
    :: PersistRecordBackend record SqlBackend
    => Key record
    -> ReaderT SqlBackend m ()

  update
    :: PersistRecordBackend record SqlBackend
    => Key record
    -> [Update record]
    -> ReaderT SqlBackend m ()
  updateGet
    :: PersistRecordBackend record SqlBackend
    => Key record
    -> [Update record]
    -> ReaderT SqlBackend m record

  -- Additional for @PersistStoreWrite@ but not part of it

  insertEntity
    :: PersistRecordBackend e SqlBackend
    => e
    -> ReaderT SqlBackend m (Entity e)
  insertRecord
    :: ( PersistEntityBackend record ~ BaseBackend SqlBackend
       , PersistEntity record
       )
    => record
    -> ReaderT SqlBackend m record



  -- @PersistUniqueRead@

  getBy
    :: PersistRecordBackend record SqlBackend
    => Unique record
    -> ReaderT SqlBackend m (Maybe (Entity record))

  -- Additional for @PersistUniqueRead@ but not part of it

  getByValue
    :: PersistRecordBackend record SqlBackend
    => record
    -> ReaderT SqlBackend m (Maybe (Entity record))
  checkUnique
    :: PersistRecordBackend record SqlBackend
    => record
    -> ReaderT SqlBackend m (Maybe (Unique record))



  -- @PersistUniqueWrite@

  deleteBy
    :: PersistRecordBackend record SqlBackend
    => Unique record
    -> ReaderT SqlBackend m ()
  insertUnique
    :: PersistRecordBackend record SqlBackend
    => record
    -> ReaderT SqlBackend m (Maybe (Key record))
  upsert
    :: PersistRecordBackend record SqlBackend
    => record
    -> [Update record]
    -> ReaderT SqlBackend m (Entity record)
  upsertBy
    :: PersistRecordBackend record SqlBackend
    => Unique record
    -> record
    -> [Update record]
    -> ReaderT SqlBackend m (Entity record)

  -- Additional for @PersistUniqueWrite@ but not part of it

  insertBy
    :: PersistRecordBackend record SqlBackend
    => record
    -> ReaderT SqlBackend m (Either (Entity record) (Key record))
  insertUniqueEntity
    :: PersistRecordBackend record SqlBackend
    => record
    -> ReaderT SqlBackend m (Maybe (Entity record))
  replaceUnique
    :: ( Eq record
       , Eq (Unique record)
       , PersistRecordBackend record SqlBackend
       )
    => Key record
    -> record
    -> ReaderT SqlBackend m (Maybe (Unique record))
  onlyUnique
    :: PersistRecordBackend record SqlBackend
    => record
    -> ReaderT SqlBackend m (Unique record)



  -- @PersistQueryRead@

  -- selectSourceRes
  --   :: (PersistRecordBackend record SqlBackend, MonadIO m1, MonadIO m2)
  --   => [Filter record]
  --   -> [SelectOpt record]
  --   -> ReaderT SqlBackend m1 (Acquire (Source m2 (Entity record)))
  selectFirst
    :: PersistRecordBackend record SqlBackend
    => [Filter record]
    -> [SelectOpt record]
    -> ReaderT SqlBackend m (Maybe (Entity record))
  -- selectKeysRes
  --   :: (MonadIO m1, MonadIO m2, PersistRecordBackend record SqlBackend)
  --   => [Filter record]
  --   -> [SelectOpt record]
  --   -> ReaderT SqlBackend m1 (Acquire (Source m2 (Key record)))
  count
    :: PersistRecordBackend record SqlBackend
    => [Filter record]
    -> ReaderT SqlBackend m Int

  -- Additional for @PersistQueryRead@ but not part of it

  -- selectSource
  --   :: ( PersistQueryRead (BaseBackend SqlBackend)
  --      , MonadResource m
  --      , PersistEntity record
  --      , PersistEntityBackend record ~ BaseBackend (BaseBackend SqlBackend)
  --      , MonadReader SqlBackend m
  --      , HasPersistBackend SqlBackend
  --      )
  --   => [Filter record]
  --   -> [SelectOpt record]
  --   -> Source m (Entity record)
  -- selectKeys
  --   :: ( PersistQueryRead (BaseBackend SqlBackend)
  --      , MonadResource m
  --      , PersistEntity record
  --      , BaseBackend (BaseBackend SqlBackend) ~ PersistEntityBackend record
  --      , MonadReader SqlBackend m
  --      , HasPersistBackend SqlBackend
  --      )
  --   => [Filter record]
  --   -> [SelectOpt record]
  --   -> Source m (Key record)
  selectList
    :: PersistRecordBackend record SqlBackend
    => [Filter record]
    -> [SelectOpt record]
    -> ReaderT SqlBackend m [Entity record]
  selectKeysList
    :: PersistRecordBackend record SqlBackend
    => [Filter record]
    -> [SelectOpt record]
    -> ReaderT SqlBackend m [Key record]



  -- @PersistQueryWrite@

  updateWhere
    :: PersistRecordBackend record SqlBackend
    => [Filter record]
    -> [Update record]
    -> ReaderT SqlBackend m ()
  deleteWhere
    :: PersistRecordBackend record SqlBackend
    => [Filter record]
    -> ReaderT SqlBackend m ()
