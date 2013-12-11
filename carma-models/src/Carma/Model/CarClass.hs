module Carma.Model.CarClass where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Types()

data CarClass = CarClass
  { ident    :: PK Int CarClass "Класс автомобиля"
  , label    :: F Text          "label" "Класс"
  } deriving Typeable

instance Model CarClass where
  type TableName CarClass = "CarClass"
  modelInfo = mkModelInfo CarClass ident
  modelView _ = defaultView
