module Carma.Model.Wazzup where

import Data.Text
import Data.Typeable
import Data.Model
import Data.Model.View

import Carma.Model.CarMake (CarMake)


data Wazzup = Wazzup
  {value :: F Text             "value"   "value"
  ,label :: F Text             "label"   "Модель"
  ,parent:: F (Ident CarMake)  "parent"  "Марка машины"
  }
  deriving Typeable


instance Model Wazzup where
  type TableName Wazzup = "Wazzup"
  modelFields = getModelFields Wazzup
  modelView _ = defaultView
