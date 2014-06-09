{-| Handler helpers. -}

module AppHandlers.Util where

import Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Pool
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Database.PostgreSQL.Simple as Pg

import Snap

import Util


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
quote x = "'" ++ (T.unpack $ T.replace "'" "''" $ T.decodeUtf8 x) ++ "'"


int :: ByteString -> String
int = T.unpack . T.decodeUtf8


mkMap :: [ByteString] -> [[Maybe ByteString]] -> [Map ByteString ByteString]
mkMap fields = map $ Map.fromList . zip fields . map (maybe "" id)


getIntParam :: ByteString -> Handler a b (Maybe Int)
getIntParam name = do
  val <- getParam name
  return $ fst <$> (B.readInt =<< val)


-- | Use a connection from a pool to run a query.
withPG :: (v -> Pool Pg.Connection)
       -> (Pg.Connection -> IO res)
       -- ^ Query action.
       -> Handler b v res
withPG pool f = gets pool >>= liftIO .(`withResource` f)


withLens :: MonadState s (Handler b v')
         => (s -> SnapletLens b v) -> Handler b v res
         -> Handler b v' res
withLens x = (gets x >>=) . flip withTop
