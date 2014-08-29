module Snaplet.DbLayer.Triggers.Defaults
  (applyDefaults
  ) where

import Prelude hiding (id)

import Control.Monad.IO.Class
import Data.Functor
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Snap
import Snaplet.Auth.Class

import Snaplet.DbLayer.Types


-- | Populate a commit with default field values.
applyDefaults :: HasAuth b
              => Object
              -> Handler b (DbLayer b) Object
applyDefaults obj = do
  -- Now actually used only by attachments
  ct <- liftIO $ round . utcTimeToPOSIXSeconds
              <$> getCurrentTime
  return $ Map.insert "ctime" (T.pack $ show ct) obj
