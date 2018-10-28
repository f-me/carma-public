{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Carma.EraGlonass.Instance.Persistent
     ( TimeoutException (..)
     ) where

import           Data.Typeable
import           Data.Either (isRight)
import qualified Data.Pool as P

import           Control.Exception.Lifted
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Concurrent.STM.TSem

import           System.Timeout (timeout)

import qualified Database.Persist.Sql as DB

import           Carma.Monad.STM
import           Carma.Monad.MVar
import           Carma.Monad.Delay
import           Carma.Monad.Thread
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
      DBConnectionPool x -> DB.runSqlPool m x
      DBConnection sem x -> do
        atomically $ waitTSem sem
        DB.runSqlConn m x `finally` atomically (signalTSem sem)

  runSqlTimeout n m =
    asks dbConnection >>= \case
      DBConnectionPool x -> fPool x
      DBConnection sem x -> do
        atomically $ waitTSem sem
        fConn x `finally` atomically (signalTSem sem)

    where
      timeoutException = toException $ TimeoutExceeded n

      fConn conn = do
        responseMVar <- newEmptyMVar

        timeoutThreadId <- fork $ do
          delay n
          putMVar responseMVar $ Left timeoutException

        dbReqThreadId <-
          DB.runSqlConn m conn `forkFinally` \case
            Left  e -> putMVar responseMVar $ Left  e
            Right x -> putMVar responseMVar $ Right x

        response <- takeMVar responseMVar
        killThread timeoutThreadId
        killThread dbReqThreadId
        pure response

      fPool connPool =
        liftIO (timeout n $ P.takeResource connPool) >>= \case
          Nothing -> pure $ Left timeoutException
          Just (conn, local) ->
            fConn conn >>= \x ->
              x <$ if isRight x
                      then liftIO $ P.putResource local conn
                      else liftIO $ P.destroyResource connPool local conn

  transactionSave = DB.transactionSave
  transactionUndo = DB.transactionUndo



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


data TimeoutException = TimeoutExceeded Int deriving (Show, Eq, Typeable)
instance Exception TimeoutException
