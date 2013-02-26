{-# LANGUAGE Rank2Types #-}
module Snaplet.DbLayer.Class where

import Control.Lens
import Snap.Snaplet

import Snaplet.DbLayer.Types

class HasDB b where
  dbLayerLens :: Simple Lens (Snaplet b) (Snaplet (DbLayer b))

withDB :: HasDB b => Handler b (DbLayer b) a -> Handler b v a
withDB = withTop' dbLayerLens
