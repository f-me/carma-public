{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model
  ( Ident(..)
  , Model(..)
  , Field, F
  , FOpt
  , FieldDesc(..)
  , fieldName
  , tableName
  , ModelFields, modelFields
  ) where


import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Data.Typeable
import GHC.TypeLits


class (SingI (TableName m), Typeable m) => Model m where
  type TableName m :: Symbol

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


data FOpt (name :: Symbol) (desc :: Symbol) = FOpt
data Field typ opt = Field
type F t n d = Field t (FOpt n d)


fieldName :: SingI name => (model -> Field typ (FOpt name desc)) -> String
fieldName (_ :: model -> Field typ (FOpt name desc))
  = fromSing (sing :: Sing name)



data FieldDesc m = FieldDesc
  {fd_fieldName :: Text
  ,fd_fieldDesc :: Text
  ,fd_fieldType :: TypeRep
  }
  deriving Show


class ModelFields m ctr where
  modelFields :: ctr -> [FieldDesc m]

instance (ModelFields m ctr, Typeable t, SingI nm, SingI desc)
    => ModelFields m (Field t (FOpt nm desc) -> ctr)
  where
    modelFields f
      = FieldDesc
        {fd_fieldName = T.pack $ fromSing (sing :: Sing nm)
        ,fd_fieldDesc = T.pack $ fromSing (sing :: Sing desc)
        ,fd_fieldType = typeOf   (undefined :: t)
        }
      : modelFields (f Field)

instance ModelFields m m where
  modelFields _ = []
