{-# LANGUAGE Rank2Types #-}
module Snaplet.SiteConfig.Config (SiteConfig(..)) where

import           Data.Map (Map)

import           Control.Lens

import qualified Data.Aeson as Aeson
import           Data.Pool
import           Database.PostgreSQL.Simple as Pg

import           Snap.Snaplet
import           Snap.Snaplet.PostgresqlSimple

import           Snaplet.DbLayer.Types (DbLayer)

import           Snaplet.SiteConfig.Models
import           Snaplet.SiteConfig.Types

data SiteConfig b = SiteConfig
  { models       :: Map ModelName Model
  , dictionaries :: Aeson.Value
  , pg_search    :: Pool Pg.Connection
  , authDb       :: Lens' b (Snaplet (DbLayer b))
  }
