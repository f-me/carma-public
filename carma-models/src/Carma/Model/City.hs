
module Carma.Model.City where


import Data.Aeson
import Data.Text
import Data.Typeable
import Database.PostgreSQL.Simple.ToField   (ToField(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))

import Data.Model
import Data.Model.View


-- FIXME: Carma.Types
data Coords = Coords -- {lon :: Double, lat :: Double}
  deriving Typeable

instance FromJSON Coords where
  parseJSON v = return Coords

instance ToJSON Coords where
  toJSON (Coords) = object []

instance ToField Coords where
  toField = undefined

instance FromField Coords where
  fromField = undefined


data City = City
  {label    :: F Text         "label"     "Название"
  ,value    :: F Text         "value"    "Внутреннее название"
--  ,coords   :: F Coords       "coords"   "Координаты"
  ,timezone :: F (Maybe Text) "timezone" "Часовой пояс"
  }
  deriving Typeable

instance Model City where
  type TableName City = "City"
  modelInfo = mkModelInfo City
  modelView _ = defaultView
