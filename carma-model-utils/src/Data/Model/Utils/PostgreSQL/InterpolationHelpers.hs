{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Model.Utils.PostgreSQL.InterpolationHelpers
     ( PlainText (..)
     , plainFieldName
     , plainTableName
     ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Text.InterpolatedString.QM
import           Data.ByteString.Builder.Prim (primMapListFixed, char8)

import           Data.Model ( Model, Field, PK, FOpt, ModelInfo (tableName)
                            , modelInfo, fieldName
                            )

import           Database.PostgreSQL.Simple.ToField

import           GHC.TypeLits (KnownSymbol)


-- | Text wrapper with a non-quoting @ToField@ instance.
--
-- Copied from the "vinnie".
newtype PlainText = PT Text

instance ToField PlainText where
  toField (PT i) = Plain $ primMapListFixed char8 $ T.unpack i


-- | Field name, unquoted.
-- "fieldPT"
plainFieldName :: KnownSymbol n => (m -> Field t (FOpt n d a)) -> PlainText
plainFieldName = PT . fieldName


-- | Table name, in double quotes.
-- "tableQT"
plainTableName :: forall m t d . Model m => (m -> PK t m d) -> PlainText
plainTableName _ = PT [qm|"{tableName (modelInfo :: ModelInfo m)}"|]
