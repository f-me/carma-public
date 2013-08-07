
module Carma.Model.City where


import Data.Text
import Data.Typeable
import Data.Aeson.Types

import Data.Model


-- FIXME: Carma.Types
data Coords = Coords -- {lon :: Double, lat :: Double}
  deriving Typeable

instance FromJSON Coords where
  parseJSON v = return Coords

data City = City
  {ident    :: F (Ident City) "id"       "id"
  ,city     :: F Text         "city"     "Название"
  ,coords   :: F Coords       "coords"   "Координаты"
  ,timezone :: F Text         "timezone" "Часовой пояс"
  }
  deriving Typeable

instance Model City where
  type TableName City = "City"
  modelFields = getModelFields City

