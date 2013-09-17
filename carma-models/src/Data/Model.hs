{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model
  ( Ident(..)
  , ident, identDesc
  , Model(..)
  , Field, F
  , FOpt
  , FieldDesc(..)
  , fieldName
  , tableName
  , getModelFields
  -- from Data.Model.View.Types
  , ModelView(..)
  ) where


import Control.Applicative
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson.Types as Aeson
import Database.PostgreSQL.Simple.FromRow (RowParser,field)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField   (ToField(..), Action)
import Data.Dynamic
import GHC.TypeLits

import Data.Model.View.Types


class (SingI (TableName m), Typeable m) => Model m where
  type TableName m :: Symbol
  modelFields :: [FieldDesc m]
  modelView :: Text -> ModelView m

tableName :: forall model . Model model => model -> String
tableName _ = fromSing (sing :: Sing (TableName model))


data Ident model = Ident {identVal :: Int}
  deriving (Typeable, Eq)

instance Model m => Show (Ident m) where
  show (Ident x :: Ident m) = "Ident " ++ modelName ++ " " ++ show x
    where
      modelName = show $ typeOf (undefined :: m)

instance FromField (Ident m) where
  fromField f x = Ident `fmap` fromField f x

instance FromJSON (Ident m) where
  parseJSON = fmap Ident . parseJSON

instance ToJSON (Ident m) where
  toJSON (Ident i) = toJSON i

instance ToField (Ident m) where
  toField (Ident i) = toField i


data FOpt (name :: Symbol) (desc :: Symbol) = FOpt
data Field typ opt = Field
type F t n d = Field t (FOpt n d)


fieldName :: SingI name => (model -> Field typ (FOpt name desc)) -> String
fieldName (_ :: model -> Field typ (FOpt name desc))
  = fromSing (sing :: Sing name)


ident :: m -> F (Ident m) "id" ""
ident _ = Field

identDesc :: forall m . Model m => FieldDesc m
identDesc = FieldDesc
  {fd_name      = "id"
  ,fd_desc      = "Object id"
  ,fd_type      = typeOf (undefined :: Ident m)
  ,fd_parseJSON = \v -> toDyn <$> (parseJSON v :: Parser (Ident m))
  ,fd_toJSON    = \d -> toJSON  (fromJust $ fromDynamic d :: Ident m)
  ,fd_fromField = toDyn <$> (field :: RowParser (Ident m))
  ,fd_toField   = \d -> toField (fromJust $ fromDynamic d :: Ident m)
  }

data FieldDesc m = FieldDesc
  {fd_name      :: Text
  ,fd_desc      :: Text
  ,fd_type      :: TypeRep
  ,fd_parseJSON :: Value -> Parser Dynamic
  ,fd_toJSON    :: Dynamic -> Value
  ,fd_fromField :: RowParser Dynamic
  ,fd_toField   :: Dynamic -> Action
  }


class GetModelFields m ctr where
  getModelFields :: ctr -> [FieldDesc m]

instance
    (GetModelFields m ctr, SingI nm, SingI desc
    ,FromJSON t, ToJSON t, FromField t, ToField t, Typeable t)
    => GetModelFields m (Field t (FOpt nm desc) -> ctr)
  where
    getModelFields f
      = FieldDesc
        {fd_name      = T.pack $ fromSing (sing :: Sing nm)
        ,fd_desc      = T.pack $ fromSing (sing :: Sing desc)
        ,fd_type      = typeOf   (undefined :: t)
        ,fd_parseJSON = \v -> toDyn <$> (parseJSON v :: Parser t)
        ,fd_toJSON    = \d -> toJSON  (fromJust $ fromDynamic d :: t)
        ,fd_fromField = toDyn <$> (field :: RowParser t)
        ,fd_toField   = \d -> toField (fromJust $ fromDynamic d :: t)
        }
      : getModelFields (f Field)

instance GetModelFields m m where
  getModelFields _ = []
