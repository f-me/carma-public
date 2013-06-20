{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model.Sql where

import Data.Typeable
import GHC.TypeLits

import Data.Model

mkSelectDictQuery
  :: (SingI name, Typeable model)
  => (model -> Field name typ) -> String
mkSelectDictQuery f
  = "SELECT id::text, " ++ fieldName f ++ "::text"
  ++ " FROM " ++ show (modelName f)
