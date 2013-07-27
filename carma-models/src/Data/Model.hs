{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model
  ( Ident(..)
  , Model(..)
  , Field
  , F
  , FOpt
  , fieldName
  , tableName
  ) where


import Database.PostgreSQL.Simple.FromField (FromField(..))
import Data.Typeable
import GHC.TypeLits


class (SingI (TableName m), Typeable m) => Model m where
  type TableName m :: Symbol

tableName :: Model model => model -> String
tableName (_ :: model)
  = fromSing (sing :: Sing (TableName model))


data Ident model = Ident {identVal :: Int}
  deriving Eq

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
