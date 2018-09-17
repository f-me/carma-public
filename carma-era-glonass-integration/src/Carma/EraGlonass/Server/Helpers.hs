{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Carma.EraGlonass.Server.Helpers
     ( inBackground
     , runSqlProtected
     , runSqlInTime
     , getRandomResponseId
     ) where

import           Data.Monoid ((<>))
import           Data.String (fromString)
import           Data.Text (Text)
import           Text.InterpolatedString.QM

import           Control.Monad
import           Control.Monad.Error.Class (MonadError, throwError, catchError)
import           Control.Monad.Reader (MonadReader, asks, ReaderT)
import           Control.Monad.Random.Class (MonadRandom (..))
import           Control.Exception (SomeException)
import           Control.Concurrent.STM.TVar

import           Servant

import           Database.Persist.Sql (SqlBackend)

import           Carma.Utils.Operators
import           Carma.Monad.STM
import           Carma.Monad.Thread
import           Carma.Monad.LoggerBus
import           Carma.Monad.PersistentSql
import           Carma.EraGlonass.Types
import           Carma.EraGlonass.Instances ()


inBackground
  :: ( MonadReader AppContext m
     , MonadError ServantErr m
     , MonadThread m
     , MonadSTM m
     )
  => m ()
  -> m ()

inBackground m = do
  counter <- asks backgroundTasksCounter
  atomically $ modifyTVar' counter succ
  void $ fork $ do
    m `catchError` \_ -> pure () -- Ignoring @ServantErr@ exception
    atomically $ modifyTVar' counter pred


runSqlProtected
  :: ( MonadReader AppContext m
     , MonadLoggerBus m
     , MonadPersistentSql m
     , MonadError ServantErr m
     )
  => Text -- ^ Fail message
  -> ReaderT SqlBackend m a
  -> m a

runSqlProtected errMsg =
  runSqlInTime >=> \case
    Right x -> pure x
    Left  e -> do
      let logMsg = [qmb| Database request is failed: {errMsg}
                         Exception: {e} |]

      logError [qm| {logMsg} |]
      throwError err500 { errBody = logMsg }


runSqlInTime
  :: (MonadReader AppContext m, MonadPersistentSql m)
  => ReaderT SqlBackend m a
  -> m (Either SomeException a)

runSqlInTime m = asks dbRequestTimeout >>= flip runSqlTimeout m


getRandomResponseId :: MonadRandom m => m Text
getRandomResponseId =
  getRandomRs (0, pred $ length randomChars)
    <&> fmap (randomChars !!) ? take responseIdLength ? fromString
  where
    randomChars = ['a'..'z'] <> ['0'..'9'] :: String
    responseIdLength = 50 -- Chars count
