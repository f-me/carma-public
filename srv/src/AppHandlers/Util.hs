
module AppHandlers.Util where


import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)

import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.ToField

import Data.Aeson as Aeson

import Snap

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


handleError :: MonadSnap m => Int -> m ()
handleError err = do
    modifyResponse $ setResponseCode err
    getResponse >>= finishWith


toBool :: ByteString -> String
toBool "1" = "true"
toBool _   = "false"

quote :: ByteString -> String
quote x = "'" ++ (replace "'" "''" $ T.unpack (T.decodeUtf8 x)) ++ "'"

int :: ByteString -> String
int = T.unpack . T.decodeUtf8

mkMap :: [ByteString] -> [[Maybe ByteString]] -> [Map ByteString ByteString]
mkMap fields = map $ Map.fromList . zip fields . map (maybe "" id)


-- | Apply a function to a 'Maybe' value, producing a pair with True
-- if Nothing is provided and False otherwise. Similar to 'maybe'.
--
-- This is handy when used with Postgres 'query' in order to support
-- optional select query conditions which are ignored when Nothing is
-- provided:
--
-- > mval <- getParam "someParam"
-- > query "SELECT * FROM foo WHERE (? AND field = ?);"
-- >       (sqlFlagPair ""::ByteString id mval)
sqlFlagPair :: b 
            -> (a -> b)
            -> Maybe a
            -> (Bool, b)
sqlFlagPair def _ Nothing  = (True,  def)
sqlFlagPair _   f (Just v) = (False, f v)
