

module Carma.Model.CarModel where

import Data.Text
import Data.Typeable
import Data.Model
import Data.Model.View

import Carma.Model.CarMake (CarMake)


data CarModel = CarModel
  {ident :: PK Int CarModel ""
  ,value :: F Text             "value"   "value"
  ,label :: F Text             "label"   "Модель"
  ,info  :: F Text             "info"    "Информация о модели"
  ,parent:: F (IdentI CarMake) "parent"  "Марка машины"
  }
  deriving Typeable


instance Model CarModel where
  type TableName CarModel = "CarModel"
  modelInfo = mkModelInfo CarModel ident
  modelView _ = modifyView defaultView
    [textarea info]
