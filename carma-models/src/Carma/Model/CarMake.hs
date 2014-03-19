
module Carma.Model.CarMake where

import Data.Text
import Data.Typeable
import Data.Vector

import Data.Model
import Data.Model.View

import Carma.Model.Types()
import Carma.Model.PgTypes()

data CarMake = CarMake
  { ident    :: PK Int CarMake ""
  , value    :: F Text "value" "value"
  , label    :: F Text "label" "Марка"
  , synonyms :: F (Maybe (Vector Text)) "synonyms" "Синонимы"
  }
  deriving Typeable


instance Model CarMake where
  type TableName CarMake = "CarMake"
  modelInfo = mkModelInfo CarMake ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
