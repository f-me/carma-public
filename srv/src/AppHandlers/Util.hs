
module AppHandlers.Util where


import Snap
import Snap.Snaplet

import Data.Aeson as Aeson

import Application
import Util


------------------------------------------------------------------------------
-- | Utility functions
writeJSON :: Aeson.ToJSON v => v -> AppHandler ()
writeJSON v = do
  modifyResponse $ setContentType "application/json"
  writeLBS $ Aeson.encode v

getJSONBody :: Aeson.FromJSON v => AppHandler v
getJSONBody = Util.readJSONfromLBS <$> readRequestBody 4096
