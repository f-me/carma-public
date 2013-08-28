
{-# LANGUAGE Rank2Types #-}
module Carma.Model
  (Model
  ,Ident(..)
  ,dispatch
  ) where

import Data.Text (Text)
import Data.Model

import Carma.Model.Dictionary (Dictionary)
import Carma.Model.CarMake    (CarMake)
import Carma.Model.CarModel   (CarModel)


dispatch :: Text -> (forall m . Model m => m -> a) -> Maybe a
dispatch model fn = case model of
  "Dictionary" -> Just $ fn (undefined :: Dictionary)
  "CarMake"    -> Just $ fn (undefined :: CarMake)
  "CarModel"   -> Just $ fn (undefined :: CarModel)
  _ -> Nothing

