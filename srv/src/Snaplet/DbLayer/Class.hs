
module Snaplet.DbLayer.Class where

import Data.Lens.Common
import Snap.Snaplet

import Snaplet.DbLayer.Types

class HasDB b where
  dbLayerLens :: Lens (Snaplet b) (Snaplet (DbLayer b))

withDB :: HasDB b => Handler b (DbLayer b) a -> Handler b v a
withDB = withTop' dbLayerLens
