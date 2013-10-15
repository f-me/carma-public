
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model.Patch
  (Patch, untypedPatch
  ,get
  ) where


import Control.Applicative
import Data.Aeson.Types
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)

import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow

import Data.Maybe (fromJust)
import Data.Dynamic
import GHC.TypeLits

import Data.Model


data Patch m = Patch {untypedPatch :: HashMap Text Dynamic}

get
  :: (Typeable t, SingI name)
  => Patch m -> (m -> Field t (FOpt name desc)) -> Maybe t
get (Patch m) f
  = fromJust . fromDynamic
  <$> HashMap.lookup (fieldName f) m


instance Model m => FromJSON (Patch m) where
  parseJSON (Object o)
    = Patch . HashMap.fromList
    <$> mapM parseField (HashMap.toList o)
    where
      fields = modelFieldsMap (modelInfo :: ModelInfo m)
      parseField (name, val) = case HashMap.lookup name fields of
        Nothing -> fail $ "Unexpected field: " ++ show name
        Just p  -> (name,) <$> fd_parseJSON p val
  parseJSON j = fail $ "JSON object expected but here is what I have: " ++ show j


instance Model m => ToJSON (Patch m) where
  toJSON (Patch m) = object [(k, toJS k v) | (k,v) <- HashMap.toList m]
    where
      fields = modelFieldsMap (modelInfo :: ModelInfo m)
      toJS k = fd_toJSON $ fields HashMap.! k


instance Model m => FromRow (Patch m) where
  fromRow = Patch . HashMap.fromList <$> sequence
    [(fd_name f,) <$> fd_fromField f
    | f <- modelFields (modelInfo :: ModelInfo m)
    ]

instance Model m => ToRow (Patch m) where
  toRow (Patch m) = map fieldToRow $ HashMap.toList m
    where
      fields = modelFieldsMap (modelInfo :: ModelInfo m)
      fieldToRow (nm, val) = case HashMap.lookup nm fields of
        Nothing -> error $ "BUG: unexpected field " ++ show nm
        Just f  -> fd_toField f val
