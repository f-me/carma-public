module Carma.Model.CarGeneration where

import Data.Text
import Data.Typeable
import Data.Vector

import Data.Model
import Data.Model.Types
import Data.Model.View

import Carma.Model.CarModel (CarModel)
import Carma.Model.Search (searchView, one)


data CarGeneration = CarGeneration
  { ident    :: PK Int CarGeneration ""
  , label    :: F Text             "label"    "Поколение"
  , parent   :: F (IdentI CarModel) "parent"   "Модель машины"
  , synonyms :: F (Maybe (Vector Text)) "synonyms" "Синонимы"
  }
  deriving Typeable


instance Model CarGeneration where
  type TableName CarGeneration = "CarGeneration"
  modelInfo = mkModelInfo CarGeneration ident
  modelView = \case
    ""        -> Just $ modifyView defaultView
      [ required label
      , required parent
      ]
    "parents" -> Just
      $ (modifyView (searchView [("parent", one parent)])
         [useSubprefixedParent])
        {mv_modelName = "CarGeneration"}
    _  -> Nothing

useSubprefixedParent :: (Text, FieldView -> FieldView) :@ CarGeneration
useSubprefixedParent =
  dict parent $
  (dictOpt "prefixedModels"){dictType = Just "ComputedDict", dictBounded = True}
