{-# LANGUAGE FlexibleContexts, OverloadedStrings, QuasiQuotes, TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Carma.EraGlonass.Helpers
     ( inBackground
     , runSqlInTime
     , (#), type (#)
     ) where


import           Text.InterpolatedString.QM

import           Control.Monad (void)
import           Control.Monad.Reader (MonadReader, ReaderT, asks)
import           Control.Monad.Catch (MonadCatch (catch))
import           Control.Concurrent.STM.TVar
import           Control.Exception
                   ( SomeException (SomeException)
                   , displayException
                   )

import           Database.Persist.Sql (SqlBackend)

import           Servant ((:<|>) ((:<|>)))

import           Carma.Monad.STM
import           Carma.Monad.Thread
import           Carma.Monad.LoggerBus
import           Carma.Monad.PersistentSql
import           Carma.EraGlonass.Types.AppContext (AppContext (..))
import           Carma.EraGlonass.Instances ()


type (#) = (:<|>)
(#) = (:<|>)
(#) :: a -> b -> (:<|>) a b
infixr 3 #
{-# INLINE (#) #-}


inBackground
  ::
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadThread m
   , MonadCatch m
   , MonadSTM m
   )
  => m ()
  -> m ()

inBackground m = do
  counter <- asks backgroundTasksCounter
  atomically $ modifyTVar' counter succ
  void $ fork $ do
    m `catch` \(SomeException e) ->
      void $ fork $ logErrorS "BackgroundTask" [qms|
        Some background task is unexpectedly failed with exception:
        {displayException e}
      |]

    atomically $ modifyTVar' counter pred


runSqlInTime
  :: (MonadReader AppContext m, MonadPersistentSql m)
  => ReaderT SqlBackend m a
  -> m (Either SomeException a)

runSqlInTime m = asks dbRequestTimeout >>= flip runSqlTimeout m
