module Carma.Model.Wazzup where

import Data.Text
import Data.Typeable
import Data.Model
import Data.Model.View

import Carma.Model.CarMake (CarMake)


data Wazzup = Wazzup
  {value :: F (IdentT Wazzup)  "value"   "value"
  ,label :: F Text             "label"   "Модель"
  ,parent:: F (IdentI CarMake) "parent"  "Марка машины"
  }
  deriving Typeable


instance Model Wazzup where
  type TableName Wazzup = "Wazzup"
  modelInfo = mkModelInfo Wazzup value
  modelView _ = defaultView
