{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}

module Carma.EraGlonass.Instance.Persistent
     ( MonadPersistentSql
     , DBAction
     , runSqlTimeout
     , TimeoutException (..)
     ) where

import           Data.Typeable
import           Data.Either (isRight)
import qualified Data.Pool as P

import           Control.Monad.Except
import           Control.Exception.Lifted
import           Control.Monad.Reader hiding (local)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Concurrent.Lifted
import           Control.Concurrent.STM.TSem

import           System.Timeout (timeout)

import           Servant (ServerError)
import qualified Database.Persist.Sql as DB

import           Carma.Monad.Thread
import           Carma.EraGlonass.Types.AppContext
                   ( AppContext (dbConnection)
                   , DBConnection (..)
                   )


type MonadPersistentSql m =
         ( Monad m
         , MonadIO m
         , MonadBaseControl IO m
         , MonadReader AppContext m
         )

type DBAction a
  = ReaderT DB.SqlBackend (ReaderT AppContext (ExceptT ServerError IO)) a

-- | To prevent MonadUnliftIO constraint from bubbling up,
-- we drop to IO and then build monad stack enough to run db actions.
liftDBA
  :: AppContext -> DBAction a
  -> ReaderT DB.SqlBackend IO (Either ServerError a)
liftDBA cxt m = do
  bk <- ask
  liftIO $ runExceptT $ runReaderT (runReaderT m bk) cxt

runSqlTimeout
  :: MonadPersistentSql m
  => Int -> DBAction a -> m (Either SomeException a)
runSqlTimeout n m = do
  cxt <- ask
  let m' = liftDBA cxt m
  res <- liftIO $ case dbConnection cxt of
    DBConnectionPool x -> fPool m' x
    DBConnection sem x -> do
      atomically $ waitTSem sem
      fConn m' x `finally` atomically (signalTSem sem)
  return $ res >>= \case
    Left err -> Left $ toException err
    Right r -> Right r

  where
    timeoutException = toException $ TimeoutExceeded n

    fConn dbAct conn = do
      responseMVar <- newEmptyMVar

      timeoutThreadId <- fork $ do
        delay n
        putMVar responseMVar $ Left timeoutException

      dbReqThreadId <-
        DB.runSqlConn dbAct conn `forkFinally` \case
          Left  e -> putMVar responseMVar $ Left  e
          Right x -> putMVar responseMVar $ Right x

      response <- takeMVar responseMVar
      killThread timeoutThreadId
      killThread dbReqThreadId
      pure response

    fPool dbAct connPool =
      (timeout n $ P.takeResource connPool) >>= \case
        Nothing -> pure $ Left timeoutException
        Just (conn, local) ->
          fConn dbAct conn >>= \x ->
            x <$ if isRight x
                    then P.putResource local conn
                    else P.destroyResource connPool local conn


data TimeoutException = TimeoutExceeded Int deriving (Show, Eq, Typeable)
instance Exception TimeoutException
