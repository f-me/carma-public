
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model.Patch
  (Patch
  ,get
  ) where


import Control.Applicative
import Data.Aeson.Types
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Data.Dynamic
import GHC.TypeLits

import Data.Model


newtype Patch m = Patch (HashMap Text Dynamic)

get
  :: (Typeable t, SingI name)
  => Patch m -> (m -> Field t (FOpt name desc)) -> Maybe t
get (Patch m) f
  = fromJust . fromDynamic
  <$> HashMap.lookup (T.pack $ fieldName f) m


instance Model m => FromJSON (Patch m) where
  parseJSON (Object o)
    = Patch . HashMap.fromList
    <$> mapM parseField (HashMap.toList o)
    where
      fields :: HashMap Text (FieldDesc m)
      fields = HashMap.fromList [(fd_name f, f) | f <- modelFields]
      parseField (name, val) = case HashMap.lookup name fields of
        Nothing -> fail $ "Unexpected field: " ++ show name
        Just p  -> (name,) <$> (fd_parseJSON p) val
  parseJSON j = fail $ "JSON object expected but here is what I have: " ++ show j


instance Model m => ToJSON (Patch m) where
  toJSON (Patch m) = object [(k, toJS k v) | (k,v) <- HashMap.toList m]
    where
      fields :: HashMap Text (FieldDesc m)
      fields = HashMap.fromList [(fd_name f, f) | f <- modelFields]
      toJS k = fd_toJSON $ fields HashMap.! k


