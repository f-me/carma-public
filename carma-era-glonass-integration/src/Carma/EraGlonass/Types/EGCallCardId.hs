{-# LANGUAGE OverloadedStrings, OverloadedLists, QuasiQuotes, LambdaCase #-}
{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, TypeFamilies, InstanceSigs #-}

module Carma.EraGlonass.Types.EGCallCardId
     ( EGCallCardId (..)
     ) where

import           GHC.Generics

import           Data.Proxy
import           Data.Function ((&))
import           Data.Text
import           Text.InterpolatedString.QM
import           Data.String (IsString (fromString))
import           Data.Aeson
import           Data.Aeson.Types (Parser, typeMismatch)
import           Data.Swagger
import           Data.Swagger.Declare (Declare)
import           Data.Attoparsec.Text hiding (Parser)

import           Database.PostgreSQL.Simple.FromField
                   ( FromField (..)
                   , fromJSONField
                   )
import           Database.PostgreSQL.Simple.ToField
                   ( ToField (..)
                   , toJSONField
                   )
import           Database.Persist.Sql (PersistFieldSql (sqlType))
import           Database.Persist.Types (SqlType (..), PersistValue (..))
import           Database.Persist.Class
                   ( PersistField (toPersistValue, fromPersistValue)
                   , fromPersistValueJSON
                   )

import           Data.Model
import           Data.Model.Types

import           Carma.Utils.TypeSafe.Generic.DataType


newtype EGCallCardId
      = EGCallCardId { fromEGCallCardId :: Text }
        deriving (Eq, Show, Generic)

instance IsString EGCallCardId where
  fromString = EGCallCardId . fromString

instance ToJSON EGCallCardId where
  toJSON = String . fromEGCallCardId

instance FromJSON EGCallCardId where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  parseJSON :: forall t. t ~ EGCallCardId => Value -> Parser t

  parseJSON v@(String x)
    = parseOnly parser x
    & \case Left  _ -> typeMismatch (typeName (Proxy :: Proxy t)) v
            Right y -> pure y

    where parser = EGCallCardId . fromString <$> many1 anyChar <* endOfInput

  parseJSON invalid = typeMismatch (typeName (Proxy :: Proxy t)) invalid

instance ToSchema EGCallCardId where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  declareNamedSchema
    :: forall proxy t. t ~ EGCallCardId
    => proxy t
    -> Declare (Definitions Schema) NamedSchema

  declareNamedSchema _ = pure $ NamedSchema (Just typeName'') mempty
    { _schemaParamSchema = mempty
        { _paramSchemaType      = SwaggerString
        , _paramSchemaPattern   = Just [qn| .+ |]
        , _paramSchemaMinLength = Just 1
        }
    }
    where typeName'' = typeName (Proxy :: Proxy t)

instance PersistField EGCallCardId where
  toPersistValue = PersistText . fromEGCallCardId
  fromPersistValue = fromPersistValueJSON

instance PersistFieldSql EGCallCardId where
  sqlType Proxy = SqlString

instance FromField EGCallCardId where
  fromField = fromJSONField

instance ToField EGCallCardId where
  toField = toJSONField

instance PgTypeable EGCallCardId where
  pgTypeOf _ = PgType "text" True

instance DefaultFieldView EGCallCardId where
  defaultFieldView f
    = FieldView
    { fv_name = fieldName f
    , fv_type = "text"
    , fv_canWrite = False
    , fv_meta =
        [ ("label", String $ fieldDesc f)
        , ("app",   String $ fieldKindStr $ fieldToFieldKindProxy f)
        ]
    } where fieldToFieldKindProxy :: (m -> FF t nm desc a) -> Proxy a
            fieldToFieldKindProxy _ = Proxy
