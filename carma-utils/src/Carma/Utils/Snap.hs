module Carma.Utils.Snap where

import           Control.Monad.State.Class
import qualified Control.Exception as Ex
import           Data.Aeson as Aeson
import           Data.Attoparsec.ByteString.Lazy (Result(..))
import qualified Data.Attoparsec.ByteString.Lazy as Atto
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as L
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import           Data.Typeable

import Snap

-- | Use the supplied parser to read a parameter from request. Fail
-- when the parameter is not present or can't be parsed.
parseMayParam :: MonadSnap m =>
                 Atto.Parser a
              -> ByteString
              -- ^ Parameter name.
              -> m (Maybe a)
parseMayParam parser k = do
  input <- fmap (Atto.parseOnly parser) <$> getParam k
  return $ case input of
             Just (Right p) -> Just p
             _ -> Nothing

getJSONBody :: Aeson.FromJSON v => Handler a b v
getJSONBody = readJSONfromLBS <$> readRequestBody (4 * 1024 * 1024)

data JSONParseException
  = AttoparsecError FilePath String
  | FromJSONError FilePath String
  deriving (Show, Typeable)

instance Ex.Exception JSONParseException


readJSON :: FromJSON v => FilePath -> IO v
readJSON f = readJSONfromLBS' f `fmap` L.readFile f

readJSONfromLBS :: FromJSON v => L.ByteString -> v
readJSONfromLBS = readJSONfromLBS' "LBS"

readJSONfromLBS' :: FromJSON v => String -> L.ByteString -> v
readJSONfromLBS' src s
  = case Atto.parse Aeson.json' s of
    Done _ jsn -> case Aeson.fromJSON jsn of
      Success t -> t
      Error err -> Ex.throw $ FromJSONError src err
    err -> Ex.throw $ AttoparsecError src (show err)

writeJSON :: ToJSON v => v -> Snap.Handler a b ()
writeJSON v = do
  modifyResponse $ setContentType "application/json"
  writeLBS $ encode v

mbreadInt :: Text -> Maybe Int
mbreadInt s = case T.decimal s of
  Right (i, "") -> Just i
  _             -> Nothing

mbreadDouble :: Text -> Maybe Double
mbreadDouble s =  case T.double s of
  Right (i,"") -> Just i
  _            -> Nothing

readDouble :: Text -> Double
readDouble = fromMaybe 0 . mbreadDouble




handleError :: MonadSnap m => Int -> m ()
handleError err = do
    modifyResponse $ setResponseCode err
    getResponse >>= finishWith


quote :: ByteString -> String
quote x = "'" ++ (T.unpack $ T.replace "'" "''" $ T.decodeUtf8 x) ++ "'"


mkMap :: [Text] -> [[Maybe Text]] -> [Map Text Text]
mkMap fields = map $ Map.fromList . zip fields . map (fromMaybe "")


getParamT :: ByteString -> Handler a b (Maybe Text)
getParamT = fmap (fmap T.decodeUtf8) . getParam


getIntParam :: ByteString -> Handler a b (Maybe Int)
getIntParam name = do
  val <- getParam name
  return $ fst <$> (B.readInt =<< val)


withLens :: MonadState s (Handler b v')
         => (s -> SnapletLens b v) -> Handler b v res
         -> Handler b v' res
withLens x = (gets x >>=) . flip withTop
