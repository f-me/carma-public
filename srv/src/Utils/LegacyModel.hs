module Utils.LegacyModel where

import Data.Aeson
import Data.ByteString.Char8 (readInt)
import Data.Model

import Snaplet.DbLayer.Types (ObjectId)


readIdent :: ObjectId -> IdentI m
readIdent bs = case readInt bs of
                 Just (n, _) -> Ident n
                 Nothing     -> error "readIdent: no integer"

recode :: (FromJSON t, ToJSON f) => f -> t
recode o = case decode $ encode o of
             Just v -> v
             Nothing -> error "recode: JSON conversion failed"
