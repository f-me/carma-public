{-# LANGUAGE ScopedTypeVariables #-}

module Utils.Model
  ( PlainText (..)
  , identFv
  , fvIdent
  , fvIdentBs
  , fieldPT
  , tableQT
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T

import           Data.ByteString.Char8 (ByteString)

import qualified Data.Model as Model

import qualified Blaze.ByteString.Builder.Char8 as BZ
import           Database.PostgreSQL.Simple.ToField

import           GHC.TypeLits (KnownSymbol)


-- | Convert an Ident to an untyped field value.
identFv :: Model.Model m => Model.IdentI m -> Text
identFv (Model.Ident v) = T.pack $ show v


-- | Convert an untyped field value to an Ident if it's a numeric
-- string.
fvIdent :: Model.Model m => Text -> Maybe (Model.IdentI m)
fvIdent s = case T.decimal s of
  Right (i, _) -> Just $ Model.Ident i
  _            -> Nothing

-- | Same as `fvIdent` but for `ByteString`
fvIdentBs :: Model.Model m => ByteString -> Maybe (Model.IdentI m)
fvIdentBs = fvIdent . T.decodeUtf8

-- | Text wrapper with a non-quoting 'ToField' instance.
--
-- Copied from vinnie.
newtype PlainText = PT Text

instance ToField PlainText where
  toField (PT i) = Plain $ BZ.fromText i


-- | Field name, unquoted.
fieldPT :: KnownSymbol n => (m -> Model.Field t (Model.FOpt n d a)) -> PlainText
fieldPT = PT . Model.fieldName


-- | Table name, in double quotes.
tableQT :: forall m t d. Model.Model m => (m -> Model.PK t m d) -> PlainText
tableQT _ = PT $ T.concat
  ["\"", Model.tableName (Model.modelInfo :: Model.ModelInfo m), "\""]
