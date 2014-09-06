module Carma.Model.City where

import Data.Text (Text)
import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.Types
import Carma.Model.PgTypes()


data City = City
  {ident    :: PK Int City ""
  ,label    :: F Text         "label"    "Название"
  ,value    :: F Text         "value"    "Название транслитом"
  ,coords   :: F (Maybe Coords) "coords" "Координаты"
  ,timezone :: F Text "timezone" "Часовой пояс"
  }
  deriving Typeable

instance Model City where
  type TableName City = "City"
  modelInfo = mkModelInfo City ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
