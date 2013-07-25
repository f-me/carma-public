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


import qualified Database.PostgreSQL.Simple.FromField as Pg
import Data.Typeable
import GHC.TypeLits


data Ident model = Ident {identVal :: Int}
  deriving Eq

instance Model m => Show (Ident m) where
  show (Ident x :: Ident m) = "Ident " ++ modelName ++ " " ++ show x
    where
      modelName = show $ typeOf (undefined :: m)

instance Pg.FromField (Ident m) where
  fromField f x = Ident `fmap` Pg.fromField f x

data FOpt (name :: Symbol) (desc :: Symbol) = FOpt
data Field typ opt = Field
type F t n d = Field t (FOpt n d)

class Typeable m => Model m where
  type TableName m :: Symbol

fieldName :: SingI name => (model -> Field typ (FOpt name desc)) -> String
fieldName (_ :: model -> Field typ (FOpt name desc))
  = fromSing (sing :: Sing name)


tableName
  :: (Model model, SingI (TableName model))
  => (model -> Field typ opt) -> String
tableName (_ :: model -> Field typ opt)
  = fromSing (sing :: Sing (TableName model))
