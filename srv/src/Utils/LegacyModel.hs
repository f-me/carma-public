{-# LANGUAGE ScopedTypeVariables #-}
module Utils.LegacyModel where

import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Aeson
import           Data.Model

import           Snaplet.DbLayer.Types (ObjectId)
import           Util


readIdent :: Model m => ObjectId -> IdentI m
readIdent s = fromMaybe (error "readIdent: no integer") $ fvIdent s

recode :: (FromJSON t, ToJSON f) => f -> t
recode o = case decode $ encode o of
             Just v -> v
             Nothing -> error "recode: JSON conversion failed"

mkLegacyIdent :: forall m.Model m => IdentI m -> Text
mkLegacyIdent idt = T.concat [mdl, ":", T.pack $ show $ identVal idt]
    where mdl = modelName (modelInfo :: ModelInfo m)
