{-# LANGUAGE FlexibleContexts #-}

module Snaplet.DbLayer.Util
    (selectDb)

where

import Data.Pool
import Database.PostgreSQL.Simple.FromRow

import Snap
import Snap.Snaplet.PostgresqlSimple ( Postgres
                                     , getPostgresState
                                     , pgPool)

import Data.Model.Sql (SqlQ(..), select)

import Snaplet.DbLayer.Types

-- | Select using carma-models.
selectDb :: (FromRow (QRes q), SqlQ q) => q -> Handler b (DbLayer b) [QRes q]
selectDb q = do
  s <- getPostgresState
  liftIO $ withResource (pgPool s) $ select q
