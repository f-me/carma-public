{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model
  ( Field
  , fieldName
  , modelName
  , ident
  ) where


import Data.Typeable
import GHC.TypeLits


data Ident model = Ident Int
data Field (name :: Symbol) typ = Field


fieldName :: SingI name => (model -> Field name typ) -> String
fieldName (_ :: model -> Field name typ)
  = fromSing (sing :: Sing name)


modelName :: Typeable model => (model -> Field name typ) -> String
modelName (_ :: model -> Field name typ)
  = tyConName $ typeRepTyCon $ typeOf (undefined :: model)


ident :: Field "id" (Ident model)
ident =  Field

