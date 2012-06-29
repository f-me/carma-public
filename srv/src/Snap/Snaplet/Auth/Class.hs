
module Snap.Snaplet.Auth.Class where

import Data.Lens.Common
import Snap.Snaplet
import Snap.Snaplet.Auth


class HasAuth b where
  authLens :: Lens (Snaplet b) (Snaplet (AuthManager b))

withAuth :: HasAuth b => Handler b (AuthManager b) a -> Handler b v a
withAuth = withTop' authLens
