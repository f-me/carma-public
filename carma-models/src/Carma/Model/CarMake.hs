
module Carma.Model.CarMake where

import Data.Text
import Data.Typeable
import Data.Model
import Data.Model.View


data CarMake = CarMake
  {value :: F Text            "value" "value"
  ,label :: F Text            "label" "Марка"
  }
  deriving Typeable


instance Model CarMake where
  type TableName CarMake = "CarMake"
  modelFields = getModelFields CarMake


view = defaultView :: View CarMake
