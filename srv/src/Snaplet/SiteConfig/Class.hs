{-# LANGUAGE Rank2Types #-}
module Snaplet.SiteConfig.Class where

import Snap.Snaplet
import Snaplet.SiteConfig


class HasSiteConfig b where
  siteConfigLens :: SnapletLens (Snaplet b) (SiteConfig b)


withSiteConfig :: HasSiteConfig b => Handler b (SiteConfig b) a -> Handler b v a
withSiteConfig = withTop' siteConfigLens
