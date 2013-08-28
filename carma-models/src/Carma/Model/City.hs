
module Carma.Model.City where


import Data.Aeson
import Data.Text
import Data.Typeable
import Database.PostgreSQL.Simple.ToField   (ToField(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))

import Data.Model


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
  {city     :: F Text         "city"     "Название"
  ,coords   :: F Coords       "coords"   "Координаты"
  ,timezone :: F Text         "timezone" "Часовой пояс"
  }
  deriving Typeable

instance Model City where
  type TableName City = "City"
  modelFields = getModelFields City
