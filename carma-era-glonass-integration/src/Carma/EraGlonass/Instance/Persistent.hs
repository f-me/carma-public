{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}

module Carma.EraGlonass.Instance.Persistent
     ( MonadPersistentSql
     , runSql
     , runSqlTimeout
     , TimeoutException (..)
     ) where

import           Data.Typeable
import           Data.Either (isRight)
import qualified Data.Pool as P

import           Control.Exception.Lifted
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Control.Monad.Reader (ReaderT, MonadReader, asks)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Concurrent.Lifted
import           Control.Concurrent.STM.TSem

import           System.Timeout (timeout)

import qualified Database.Persist.Sql as DB

import           Carma.Monad.Thread
import           Carma.EraGlonass.Types.AppContext
                   ( AppContext (dbConnection)
                   , DBConnection (..)
                   )

type MonadPersistentSql m =
         ( Monad m
         , MonadIO m
         , MonadUnliftIO m
         , MonadBaseControl IO m
         , MonadReader AppContext m
         )

runSql :: MonadPersistentSql m => ReaderT DB.SqlBackend m a -> m a
runSql m =
  asks dbConnection >>= \case
    DBConnectionPool x -> DB.runSqlPool m x
    DBConnection sem x -> do
      atomically $ waitTSem sem
      DB.runSqlConn m x `finally` atomically (signalTSem sem)

runSqlTimeout
  :: MonadPersistentSql m
  => Int -> ReaderT DB.SqlBackend m a -> m (Either SomeException a)
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


data TimeoutException = TimeoutExceeded Int deriving (Show, Eq, Typeable)
instance Exception TimeoutException
