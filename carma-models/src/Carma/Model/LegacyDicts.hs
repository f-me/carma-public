
module Carma.Model.LegacyDicts where

import Control.Applicative
import Data.Text
import Data.Typeable
import Data.Aeson
import Database.PostgreSQL.Simple.ToField   (ToField(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))

import Data.Model.Types
import Data.Model.CoffeeType

data Diagnosis1 = Diagnosis1 deriving Typeable
data Diagnosis2 = Diagnosis2 deriving Typeable
data Diagnosis3 = Diagnosis3 deriving Typeable
data Diagnosis4 = Diagnosis4 deriving Typeable
data Color = Color deriving Typeable

data PickerField = PickerField Text deriving Typeable
instance CoffeeType PickerField where
  coffeeType = Wrap "picker"
instance FromJSON PickerField where
  parseJSON fld = PickerField <$> parseJSON fld
instance ToJSON PickerField where
  toJSON (PickerField txt) = toJSON txt
instance ToField PickerField where
  toField (PickerField txt) = toField txt
instance FromField PickerField where
  fromField fld m = PickerField <$> fromField fld m

data MapField = MapField Text deriving Typeable
instance CoffeeType MapField where
  coffeeType = Wrap "map"
instance FromJSON MapField where
  parseJSON fld = MapField <$> parseJSON fld
instance ToJSON MapField where
  toJSON (MapField txt) = toJSON txt
instance ToField MapField where
  toField (MapField txt) = toField txt
instance FromField MapField where
  fromField fld m = MapField <$> fromField fld m


