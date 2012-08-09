
{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Util
  (readJSON
  ,readJSONfromLBS
  ,UsersDict
  ) where

import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.Maybe

import Control.Exception
import Data.Typeable
import qualified Data.ByteString.Lazy as L

import Data.Aeson as Aeson
import Data.Aeson.TH
import Data.Aeson.Types (Parser)
import Data.Attoparsec.ByteString.Lazy (Result(..))
import qualified Data.Attoparsec.ByteString.Lazy as Atto


data JSONParseException
  = AttoparsecError FilePath String
  | FromJSONError FilePath String
  deriving (Show, Typeable)

instance Exception JSONParseException


readJSON :: FromJSON v => FilePath -> IO v
readJSON f = readJSONfromLBS' f `fmap` L.readFile f

readJSONfromLBS :: FromJSON v => L.ByteString -> v
readJSONfromLBS = readJSONfromLBS' "LBS"

readJSONfromLBS' :: FromJSON v => String -> L.ByteString -> v
readJSONfromLBS' src s
  = case Atto.parse Aeson.json' s of
    Done _ jsn -> case Aeson.fromJSON jsn of
      Success t -> t
      Error err -> throw $ FromJSONError src err
    err -> throw $ AttoparsecError src (show err)

--------------------------------------------------------------------------------

data UsersDict = UsersDict [Map.Map L.ByteString L.ByteString]
                 deriving (Show)

instance FromJSON UsersDict where
  parseJSON (Object v) = do
    Array c <- v .: "uidCache"
    r <- V.mapM (parseUser) c
    return $ UsersDict $ V.toList r
      where
        parseRoles r = V.mapM (parseJSON) r >>=
                       return . (L.intercalate ",") . V.toList
        parseUser a = do
          Array u <- parseJSON a
          Object u' <- parseJSON $ u V.! 1
          value <- u' .: "login"
          Array roles <- u' .: "roles"
          roles' <- parseRoles roles
          meta  <- u' .: "meta"
          label <- meta .:? "realName" .!= ""
          return $ Map.fromList [ ("value", value)
                                , ("label", label)
                                , ("roles", roles')
                                ]

  parseJSON _ = fail "bad arg"

$(deriveToJSON id ''UsersDict)