{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Carma.EraGlonass.Persistent where

import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.Trans.Control (MonadBaseControl)

import           Database.Persist.Sql (runSqlPool)

import           Carma.Monad.PersistentSql

import           Carma.EraGlonass.Types (AppContext (dbConnectionPool))


instance ( Monad m
         , MonadReader AppContext m
         , MonadBaseControl IO m
         ) => MonadPersistentSql m
         where

  runSql m = runSqlPool m =<< asks dbConnectionPool
