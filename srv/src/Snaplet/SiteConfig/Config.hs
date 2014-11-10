{-# LANGUAGE Rank2Types #-}

module Snaplet.SiteConfig.Config (SiteConfig(..)) where

import           Control.Lens

import           Data.Map (Map)
import           Data.Text (Text)
import qualified Data.Aeson as Aeson

import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.PostgresqlSimple

import           Snaplet.Auth.Class
import           Snaplet.SiteConfig.Models
import           AppHandlers.Util


data SiteConfig b = SiteConfig
  { models       :: Map Text Model
  , dictionaries :: Aeson.Value
  , auth         :: Lens' b (Snaplet (AuthManager b))
  , db           :: Lens' b (Snaplet Postgres)
  }


instance HasPostgres (Handler b (SiteConfig b)) where
  getPostgresState = withLens db get


instance WithCurrentUser (Handler b (SiteConfig b)) where
  withCurrentUser = withLens auth currentUser
