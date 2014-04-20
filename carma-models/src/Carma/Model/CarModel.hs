module Carma.Model.CarModel where

import Data.Text
import Data.Typeable
import Data.Vector

import Data.Model
import Data.Model.View

import Carma.Model.CarMake (CarMake)
import Carma.Model.Search (searchView, one)
import Carma.Model.Types (TInt)


data CarModel = CarModel
  { ident    :: PK Int CarModel ""
  , value    :: F Text             "value"    "value"
  , label    :: F Text             "label"    "Модель"
  , info     :: F Text             "info"     "Информация о модели"
  , parent   :: F (IdentI CarMake) "parent"   "Марка машины"
  , synonyms :: F (Maybe (Vector Text)) "synonyms" "Синонимы"
  , fdds     :: F (Maybe TInt) "fdds" "FDDS-код"
  }
  deriving Typeable


instance Model CarModel where
  type TableName CarModel = "CarModel"
  modelInfo = mkModelInfo CarModel ident
  modelView = \case
    ""        -> Just $ modifyView defaultView [textarea info]
    "parents" -> Just
      $ (searchView [("parent", one parent)]) {mv_modelName = "CarModel"}
    _  -> Nothing
