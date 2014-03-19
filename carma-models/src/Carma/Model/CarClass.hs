module Carma.Model.CarClass where

import Data.Text
import Data.Typeable
import Data.Vector

import Data.Model
import Data.Model.View

import Carma.Model.Types()
import Carma.Model.PgTypes()

data CarClass = CarClass
  { ident    :: PK Int CarClass "Класс автомобиля"
  , label    :: F Text          "label" "Класс"
  , synonyms :: F (Maybe (Vector Text)) "synonyms" "Синонимы"
  } deriving Typeable

instance Model CarClass where
  type TableName CarClass = "CarClass"
  modelInfo = mkModelInfo CarClass ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
