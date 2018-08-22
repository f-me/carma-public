{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Model.Utils.LegacyModel
     (
       -- * Legacy interoperability for @Ident@s
       identToRawFieldValue
     , rawFieldValueToIdent
     , rawBSFieldValueToIdent

     , readIdent
     , mkIdentTopic
     ) where

import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import           Text.InterpolatedString.QM
import           Data.ByteString.Char8 (ByteString)

import           Data.Model


-- | Convert an @Ident@ to an untyped field value.
identToRawFieldValue :: Model m => IdentI m -> Text
identToRawFieldValue (Ident v) = [qm|{v}|]


-- | Convert an untyped field value to an @Ident@ if it's a numeric string.
rawFieldValueToIdent :: Model m => Text -> Maybe (IdentI m)
rawFieldValueToIdent s =
  case T.decimal s of
       Right (i, _) -> Just $ Ident i
       _            -> Nothing


-- | Same as @fvIdent@ but for @ByteString@.
rawBSFieldValueToIdent :: Model m => ByteString -> Maybe (IdentI m)
rawBSFieldValueToIdent = rawFieldValueToIdent . T.decodeUtf8


readIdent :: Model m => Text -> IdentI m
readIdent = fromMaybe (error "readIdent: no integer") . rawFieldValueToIdent


mkIdentTopic :: forall m. Model m => IdentI m -> Text
mkIdentTopic idt = [qm|{modelName (modelInfo :: ModelInfo m)}:{identVal idt}|]
