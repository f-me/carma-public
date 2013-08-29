
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module Carma.Model
  (Model
  ,Ident(..)
  ,dispatch
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Typeable

import Data.Model
import Carma.Model.Dictionary (Dictionary)
import Carma.Model.CarMake    (CarMake)
import Carma.Model.CarModel   (CarModel)


dispatch :: forall a . Text -> (forall m . Model m => m -> a) -> Maybe a
dispatch model fn = Map.lookup model modelMap
  where
    add :: Model m => m -> (Text, a)
    add m = (T.pack $ show $ typeOf m, fn m)
    modelMap = Map.fromList
      [add (undefined :: Dictionary)
      ,add (undefined :: CarMake)
      ,add (undefined :: CarModel)
      ]
