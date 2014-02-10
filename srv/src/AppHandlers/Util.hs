module AppHandlers.Util where


import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Data.Aeson as Aeson

import Snap

import Util
import Data.List.Utils

------------------------------------------------------------------------------
-- | Utility functions
writeJSON :: Aeson.ToJSON v => v -> Handler a b ()
writeJSON v = do
  modifyResponse $ setContentType "application/json"
  writeLBS $ Aeson.encode v

getJSONBody :: Aeson.FromJSON v => Handler a b v
getJSONBody = Util.readJSONfromLBS <$> readRequestBody 32768


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

getIntParam :: ByteString -> Handler a b (Maybe Int)
getIntParam name = do
  val <- getParam name
  return $ fst <$> (B.readInt =<< val)
