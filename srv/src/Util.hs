
{-# LANGUAGE DeriveDataTypeable #-}
module Util
  (readJSON
  ,readJSONfromLBS
  ) where

import Control.Exception
import Data.Typeable
import qualified Data.ByteString.Lazy as L

import Data.Aeson as Aeson
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

