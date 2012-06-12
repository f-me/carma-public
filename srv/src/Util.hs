
{-# LANGUAGE DeriveDataTypeable #-}
module Util
  (readJSON
  ) where

import Control.Exception
import Data.Functor
import Data.Typeable
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

import Data.Aeson as Aeson
import Data.Attoparsec.ByteString.Lazy (Result(..))
import qualified Data.Attoparsec.ByteString.Lazy as Atto


data JSONParseException
  = AttoparsecError FilePath String
  | FromJSONError FilePath String
  deriving (Show, Typeable)

instance Exception JSONParseException


readJSON :: FromJSON v => FilePath -> IO v
readJSON f = do
  res <- Atto.parse Aeson.json' <$> L.readFile f
  case res of
    Done _ jsn -> case Aeson.fromJSON jsn of
      Success t -> return t
      Error err -> throw $ FromJSONError f err
    err -> throw $ AttoparsecError f (show err)
