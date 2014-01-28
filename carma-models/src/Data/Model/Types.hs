{-# LANGUAGE ConstraintKinds #-}

module Data.Model.Types where

import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Data.Aeson.Types as Aeson
import Data.Map (Map)

import Database.PostgreSQL.Simple.FromRow   (RowParser)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField   (ToField(..), Action)

import Data.Dynamic
import GHC.TypeLits


data Wrap t a = Wrap {unWrap :: a}
type a :@ t = Wrap t a


data Ident t model = Ident {identVal :: t}
  deriving (Typeable, Eq)

type IdentI m = Ident Int m
type IdentT m = Ident Text m


instance FromField t => FromField (Ident t m) where
  fromField f x = Ident `fmap` fromField f x

instance FromJSON t => FromJSON (Ident t m) where
  parseJSON = fmap Ident . parseJSON

instance ToJSON t => ToJSON (Ident t m) where
  toJSON (Ident i) = toJSON i

instance ToField t => ToField (Ident t m) where
  toField (Ident i) = toField i


data FOpt (name :: Symbol) (desc :: Symbol) = FOpt
data Field typ opt = Field
type F t n d = Field t (FOpt n d)
type PK t m n = Field (Ident t m) (FOpt "id" n)


-- | Common constraint for higher-rank functions using field
-- accessors.
type FieldI t (n :: Symbol) (d :: Symbol) = (Typeable t, SingI n, SingI d) 


data ModelInfo m = ModelInfo
  { modelName      :: Text
  , tableName      :: Text
  , primKeyName    :: Text
  , modelFields    :: [FieldDesc]
  , modelOnlyFields:: [FieldDesc]
  , modelFieldsMap :: HashMap Text (FieldDesc)
  }

data FieldDesc = FieldDesc
  {fd_name       :: Text
  ,fd_desc       :: Text
  ,fd_type       :: TypeRep
  ,fd_parseJSON  :: Value -> Parser Dynamic
  ,fd_toJSON     :: Dynamic -> Value
  ,fd_fromField  :: RowParser Dynamic
  ,fd_toField    :: Dynamic -> Action
  ,fd_view       :: FieldView
  }


data FieldView = FieldView
  { fv_name     :: Text
  , fv_type     :: Text
  , fv_meta     :: Map Text Aeson.Value
  , fv_canWrite :: Bool
  }

instance ToJSON FieldView where
  toJSON f = object
    [ "name"     .= fv_name f
    , "type"     .= fv_type f
    , "canWrite" .= fv_canWrite f
    , "meta"     .= fv_meta f
    ]


class DefaultFieldView t where
  defaultFieldView :: (SingI nm, SingI desc) => (m -> F t nm desc) -> FieldView


data ModelView m = ModelView
  { mv_modelName :: Text
  , mv_title     :: Text
  , mv_fields    :: [FieldView]
  }

instance ToJSON (ModelView m) where
  toJSON v = object
    [ "name"      .= mv_modelName v
    , "title"     .= mv_title v
    , "fields"    .= mv_fields v
    , "canCreate" .= True
    , "canRead"   .= True
    , "canUpdate" .= True
    , "canDelete" .= True
    ]
