module Snaplet.SiteConfig.Config (SiteConfig(..)) where

import           Data.Map (Map)

import qualified Data.Aeson as Aeson
import           Data.Pool
import           Database.PostgreSQL.Simple as Pg

import           Snaplet.SiteConfig.Models
import           Snaplet.SiteConfig.Types

data SiteConfig b = SiteConfig
  {models       :: Map ModelName Model
  ,dictionaries :: Aeson.Value
  ,pg_search    :: Pool Pg.Connection
  }
