{-# LANGUAGE Rank2Types #-}

module Snaplet.SiteConfig.Config (SiteConfig(..)) where

import           Control.Lens (Lens')

import qualified Data.Aeson as Aeson

import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.PostgresqlSimple
import           Snaplet.Auth.Class
import           Carma.Utils.Snap (withLens)


data SiteConfig b = SiteConfig
  { dictionaries :: Aeson.Value
  , auth         :: Lens' b (Snaplet (AuthManager b))
  , db           :: Lens' b (Snaplet Postgres)
  }


instance HasPostgresAuth b (SiteConfig b) where
  withAuth = withLens auth
  withAuthPg = withLens db
