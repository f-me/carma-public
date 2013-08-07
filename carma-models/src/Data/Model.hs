{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model
  ( Ident(..)
  , Model(..)
  , Field, F
  , FOpt
  , FieldDesc(..)
  , fieldName
  , tableName
  , GetModelFields, getModelFields
  ) where


import Control.Applicative
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson.Types as Aeson
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Data.Dynamic
import GHC.TypeLits


class (SingI (TableName m), Typeable m) => Model m where
  type TableName m :: Symbol
  modelFields :: [FieldDesc m]

tableName :: Model model => model -> String
tableName (_ :: model)
  = fromSing (sing :: Sing (TableName model))


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


data FOpt (name :: Symbol) (desc :: Symbol) = FOpt
data Field typ opt = Field
type F t n d = Field t (FOpt n d)


fieldName :: SingI name => (model -> Field typ (FOpt name desc)) -> String
fieldName (_ :: model -> Field typ (FOpt name desc))
  = fromSing (sing :: Sing name)



data FieldDesc m = FieldDesc
  {fd_name      :: Text
  ,fd_desc      :: Text
  ,fd_type      :: TypeRep
  ,fd_parseJSON :: Value -> Parser Dynamic
  ,fd_toJSON    :: Dynamic -> Value
  }


class GetModelFields m ctr where
  getModelFields :: ctr -> [FieldDesc m]

instance
    (GetModelFields m ctr, SingI nm, SingI desc
    ,FromJSON t, ToJSON t, Typeable t)
    => GetModelFields m (Field t (FOpt nm desc) -> ctr)
  where
    getModelFields f
      = FieldDesc
        {fd_name      = T.pack $ fromSing (sing :: Sing nm)
        ,fd_desc      = T.pack $ fromSing (sing :: Sing desc)
        ,fd_type      = typeOf   (undefined :: t)
        ,fd_parseJSON = \v -> toDyn <$> (parseJSON v :: Parser t)
        ,fd_toJSON    = \d -> toJSON (fromJust $ fromDynamic d :: t)
        }
      : getModelFields (f Field)

instance GetModelFields m m where
  getModelFields _ = []
