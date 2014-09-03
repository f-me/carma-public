{-# LANGUAGE ConstraintKinds, GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model.Types where

import Control.Monad.Trans.Either (EitherT)
import Control.Exception (SomeException)
import Data.Text (Text, unpack)
import Data.HashMap.Strict (HashMap)
import Data.Aeson.Types as Aeson
import Data.Map (Map)
import Data.Int (Int64)

import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.FromRow   (RowParser)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField   (ToField(..), Action)

import Data.Dynamic

import Data.Singletons
import Data.Singletons.TypeLits


data Wrap t a = Wrap {unWrap :: a}
type a :@ t = Wrap t a


data Ident t model = Ident {identVal :: t}
  deriving (Ord, Typeable, Eq)

instance (Typeable m, Show t) => Show (Ident t m) where
  show (Ident x) = "Ident " ++ modelName ++ " " ++ show x
    where
      modelName = show $ typeOf (undefined :: m)

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


-- | FieldKind and it's singletons
data FieldKind = DefaultField | EphemeralField
data FieldKindSingleton (k :: FieldKind) where
  FKSDefault   :: FieldKindSingleton DefaultField
  FKSEphemeral :: FieldKindSingleton EphemeralField

class FieldKindSing k where fieldKindSing :: FieldKindSingleton k
instance FieldKindSing DefaultField   where fieldKindSing = FKSDefault
instance FieldKindSing EphemeralField where fieldKindSing = FKSEphemeral


data FOpt (name :: Symbol) (desc :: Symbol) (app :: FieldKind) = FOpt
data Field typ opt = Field
type FF t n d a = Field t (FOpt n d a)
type F  t n d   = FF t n d DefaultField
type EF t n d   = FF t n d EphemeralField
type PK t m n   = FF (Ident t m) "id" n DefaultField

-- | Existential wrapper for field accessors.
data FA m = forall t n d. (FieldI t n d) =>
            FA (m -> F t n d)


-- | Common constraint for higher-rank functions using field
-- accessors.
type FieldI t (n :: Symbol) (d :: Symbol) =
  ( Typeable t, PgTypeable t
  , DefaultFieldView t
  , FromJSON t, ToJSON t
  , FromField t, ToField t
  , KnownSymbol n, KnownSymbol d)


data ModelInfo m = ModelInfo
  { modelName      :: Text
  , parentName     :: Maybe Text
  , legacyModelName:: Maybe Text
  , tableName      :: Text
  , primKeyName    :: Text
  , modelFields    :: [FieldDesc]
  , modelOnlyFields:: [FieldDesc]
  , modelFieldsMap :: HashMap Text FieldDesc
  , modelCRUD      :: Maybe (CRUD m) -- ^ `Nothing` means `defaultCRUD`
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
  ,fd_pgType     :: PgType
  } | EFieldDesc
  {fd_name       :: Text
  ,fd_desc       :: Text
  ,fd_type       :: TypeRep
  ,fd_toJSON     :: Dynamic -> Value
  ,fd_parseJSON  :: Value -> Parser Dynamic
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


data CrudError
  = NoSuchObject String
  | InconsistentDbState String
  | MalformedJSON String
  | PgException SomeException
  deriving Show

type CrudRes = EitherT CrudError IO Aeson.Value

data CRUD m = CRUD
  { crud_create :: Aeson.Value             -> PG.Connection -> CrudRes
  , crud_read   :: IdentI m                -> PG.Connection -> CrudRes
  , crud_update :: IdentI m -> Aeson.Value -> PG.Connection -> CrudRes
  , crud_delete :: IdentI m                -> PG.Connection -> CrudRes
  , crud_readManyWithFilter
                :: Limit  -> Offset -> [(Text,Text)]
                                           -> PG.Connection -> CrudRes
  }

type Limit  = Int64
type Offset = Int64

class DefaultFieldView t where
  defaultFieldView
    :: (SingI nm, SingI desc, FieldKindSing a)
    => (m -> FF t nm desc a) -> FieldView


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

data PgType = PgType { pgTypeName :: Text, pgNotNull :: Bool } deriving Eq

class PgTypeable t where
  pgTypeOf :: t -> PgType

instance Show PgType where
  show PgType{..} =
    unpack pgTypeName ++ (if pgNotNull then " NOT NULL " else "")
