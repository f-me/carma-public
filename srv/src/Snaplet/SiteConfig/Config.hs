module Snaplet.SiteConfig.Config (SiteConfig(..)) where

import           Data.Map (Map)
import           Data.Text (Text)
import qualified Data.Aeson as Aeson
import           Data.Pool
import           Database.PostgreSQL.Simple as Pg
import           Snaplet.SiteConfig.Models

data SiteConfig b = SiteConfig
  { models       :: Map Text Model
  , dictionaries :: Aeson.Value
  , pg_search    :: Pool Pg.Connection
  }
