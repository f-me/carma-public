{-# LANGUAGE Rank2Types #-}

module Snaplet.SiteConfig.Config (SiteConfig(..)) where

import           Control.Lens

import qualified Data.Aeson as Aeson

import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.PostgresqlSimple

import           Snaplet.Auth.Class
import           AppHandlers.Util


data SiteConfig b = SiteConfig
  { dictionaries :: Aeson.Value
  , auth         :: Lens' b (Snaplet (AuthManager b))
  , db           :: Lens' b (Snaplet Postgres)
  }


instance HasPostgres (Handler b (SiteConfig b)) where
  getPostgresState = withLens db get


instance WithCurrentUser (Handler b (SiteConfig b)) where
  withCurrentUser = withLens auth currentUser
