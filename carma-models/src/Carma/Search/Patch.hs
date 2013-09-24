{-# LANGUAGE ScopedTypeVariables #-}

module Carma.Search.Patch where

import           Control.Applicative ((<$>))

import           Data.Maybe
import           Data.Text
import           Data.Dynamic
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (HashMap)

import           Data.Aeson (FromJSON)
import           Data.Aeson.Types

import           GHC.TypeLits

import           Data.Model

data Patch m = Patch {untypedPatch :: HashMap Text Dynamic} deriving Show

get :: (Typeable t, SingI name)
    => Patch m -> (m -> Field t (FOpt name desc)) -> Maybe t
get (Patch p) f
  = fromJust . fromDynamic <$> HM.lookup (pack $ fieldName f) p

instance Model m => FromJSON (Patch m) where
  parseJSON (Object o)
    = Patch . HM.fromList
      <$> catMaybes <$> mapM parseField (HM.toList o)
    where
      fields :: HashMap Text (FieldDesc m)
      fields = HM.fromList [(fd_name f, f) | f <- (modelFields :: [FieldDesc m])]
      parseField (name, val) = case HM.lookup name fields of
        Nothing -> return Nothing
        Just p  -> Just <$> (name,) <$> fd_parseJSON p val
  parseJSON j = fail $
                "JSON object expected but here is what I have: " ++ show j
