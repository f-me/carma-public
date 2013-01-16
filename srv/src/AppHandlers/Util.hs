
module AppHandlers.Util where


import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)

import Data.Aeson as Aeson
import Data.Time
import Control.Concurrent.STM

import Snap
import Snap.Snaplet.Auth

import Application
import Util
import Data.List.Utils

------------------------------------------------------------------------------
-- | Utility functions
writeJSON :: Aeson.ToJSON v => v -> AppHandler ()
writeJSON v = do
  modifyResponse $ setContentType "application/json"
  writeLBS $ Aeson.encode v

getJSONBody :: Aeson.FromJSON v => AppHandler v
getJSONBody = Util.readJSONfromLBS <$> readRequestBody 4096


addToLoggedUsers :: AuthUser -> AppHandler (Map Text (UTCTime,AuthUser))
addToLoggedUsers u = do
  logTVar <- gets loggedUsers
  logdUsers <- liftIO $ readTVarIO logTVar
  now <- liftIO getCurrentTime
  let logdUsers' = Map.insert (userLogin u) (now,u)
        -- filter out inactive users
        $ Map.filter ((>addUTCTime (-90*60) now).fst) logdUsers
  liftIO $ atomically $ writeTVar logTVar logdUsers'
  return logdUsers'


rmFromLoggedUsers :: AuthUser -> AppHandler ()
rmFromLoggedUsers u = do
  logdUsrs <- gets loggedUsers
  liftIO $ atomically $ modifyTVar' logdUsrs
         $ Map.delete $ userLogin u


toBool :: ByteString -> String
toBool "1" = "true"
toBool _   = "false"

quote :: ByteString -> String
quote x = "'" ++ (replace "'" "''" $ T.unpack (T.decodeUtf8 x)) ++ "'"

int :: ByteString -> String
int = T.unpack . T.decodeUtf8

mkMap :: [ByteString] -> [[Maybe ByteString]] -> [Map ByteString ByteString]
mkMap fields = map $ Map.fromList . zip fields . map (maybe "" id)
