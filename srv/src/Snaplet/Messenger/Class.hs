module Snaplet.Messenger.Class where

import Snap.Snaplet
import Snaplet.Messenger

class HasMsg b where
  messengerLens :: SnapletLens (Snaplet b) Messenger

withMsg :: HasMsg b => Handler b Messenger a -> Handler b v a
withMsg = withTop' messengerLens
