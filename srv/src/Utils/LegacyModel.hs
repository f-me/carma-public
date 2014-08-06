{-# LANGUAGE ScopedTypeVariables #-}
module Utils.LegacyModel where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import           Data.Aeson
import           Data.Model

import           Snaplet.DbLayer.Types (ObjectId)


readIdent :: ObjectId -> IdentI m
readIdent s = case T.decimal s of
                 Right (n, _) -> Ident n
                 Left _err    -> error "readIdent: no integer"

recode :: (FromJSON t, ToJSON f) => f -> t
recode o = case decode $ encode o of
             Just v -> v
             Nothing -> error "recode: JSON conversion failed"

mkLegacyIdent :: forall m.Model m => IdentI m -> Text
mkLegacyIdent idt = T.concat [mdl, ":", T.pack $ show $ identVal idt]
    where mdl = modelName (modelInfo :: ModelInfo m)
