module Snaplet.Auth.Class where

import Snap.Snaplet
import Snap.Snaplet.Auth


class HasAuth b where
  authLens :: SnapletLens (Snaplet b) (AuthManager b)

withAuth :: HasAuth b => Handler b (AuthManager b) a -> Handler b v a
withAuth = withTop' authLens

class WithCurrentUser m where
  withCurrentUser :: m (Maybe AuthUser)
