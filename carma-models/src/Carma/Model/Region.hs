
module Carma.Model.Region where

import Data.Text
import Data.Model
import Data.Typeable
import Data.Vector

import Data.Model.View
import Carma.Model.City (City)


data Region = Region
  { label  :: F Text                  "label"  "Название региона"
  , cities :: F (Vector (Ident City)) "cities" "Города в регионе"
  } deriving Typeable


instance Model Region where
  type TableName Region = "Region"
  modelInfo = mkModelInfo Region
  modelView _ = defaultView
