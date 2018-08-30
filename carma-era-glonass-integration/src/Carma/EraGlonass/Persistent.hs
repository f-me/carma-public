{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Carma.EraGlonass.Persistent where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.Trans.Control (MonadBaseControl)

import qualified Database.Persist.Sql as DB

import           Carma.Monad.PersistentSql
import           Carma.EraGlonass.Types (AppContext (dbConnectionPool))


instance ( Monad m
         , MonadIO m
         , MonadBaseControl IO m
         , MonadReader AppContext m
         ) => MonadPersistentSql m
         where

  runSql m = DB.runSqlPool m =<< asks dbConnectionPool

  -- | TODO Look at Database.Persist.Sql.Run.withResourceTimeout
  runSqlTimeout = error "Not implemented yet"

  insert = DB.insert
