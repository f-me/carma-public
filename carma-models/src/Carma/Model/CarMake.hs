
module Carma.Model.CarMake where

import Data.Text
import Data.Typeable
import Data.Model
import Data.Model.View
import Carma.Model.Types()

data CarMake = CarMake
  {ident :: PK Int CarMake ""
  ,value :: F Text "value" "value"
  ,label :: F Text "label" "Марка"
  }
  deriving Typeable


instance Model CarMake where
  type TableName CarMake = "CarMake"
  modelInfo = mkModelInfo CarMake ident
  modelView _ = defaultView
