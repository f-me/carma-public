
module Snaplet.SiteConfig.Class where

import Data.Lens.Common
import Snap.Snaplet

import Snaplet.SiteConfig


class HasSiteConfig b where
  siteConfigLens :: Lens (Snaplet b) (Snaplet (SiteConfig b))

withSiteConfig :: HasSiteConfig b => Handler b (SiteConfig b) a -> Handler b v a
withSiteConfig = withTop' siteConfigLens
