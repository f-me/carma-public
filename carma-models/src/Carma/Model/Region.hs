
module Carma.Model.Region where

import Data.Text
import Data.Model
import Data.Typeable
import Data.Vector


data Region = Region
  { label  :: F Text "label"  "Название региона"
  , cities :: F Text "cities" "Города в регионе"
  } deriving Typeable


instance Model Region where
  type TableName Region = "Region"
  modelFields = getModelFields Region
