module Carma.Model.LegacyTypes where

import Control.Applicative
import Data.Text (Text)
import Data.Typeable
import Data.Aeson
import Database.PostgreSQL.Simple.ToField   (ToField(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))

data Activity = Activity deriving Typeable
data RequestType = RequestType deriving Typeable
data ConsultationType = ConsultationType deriving Typeable
data DeliveryType = DeliveryType deriving Typeable
data TowerTypes = TowerTypes deriving Typeable
data WheelsBlockedCount = WheelsBlockedCount deriving Typeable
data UrgentServiceReason = UrgentServiceReason deriving Typeable
data CallerTypes = CallerTypes deriving Typeable
data CallTypes   = CallTypes   deriving Typeable

data JsonAsText = JsonAsText Text deriving Typeable
instance FromJSON JsonAsText where
  parseJSON fld = JsonAsText <$> parseJSON fld
instance ToJSON JsonAsText where
  toJSON (JsonAsText txt) = toJSON txt
instance ToField JsonAsText where
  toField (JsonAsText txt) = toField txt
instance FromField JsonAsText where
  fromField fld m = JsonAsText <$> fromField fld m

data Password = Password Text deriving Typeable
instance FromJSON Password where
  parseJSON fld = Password <$> parseJSON fld
instance ToJSON Password where
  toJSON (Password txt) = toJSON txt
instance ToField Password where
  toField (Password txt) = toField txt
instance FromField Password where
  fromField fld m = Password <$> fromField fld m

data Phone = Phone Text deriving (Eq, Typeable)
instance Show Phone where
  show (Phone t) = show t
instance FromJSON Phone where
  parseJSON fld = Phone <$> parseJSON fld
instance ToJSON Phone where
  toJSON (Phone txt) = toJSON txt
instance ToField Phone where
  toField (Phone txt) = toField txt
instance FromField Phone where
  fromField fld m = Phone <$> fromField fld m

data Reference = Reference Text deriving Typeable
instance FromJSON Reference where
  parseJSON fld = Reference <$> parseJSON fld
instance ToJSON Reference where
  toJSON (Reference txt) = toJSON txt
instance ToField Reference where
  toField (Reference txt) = toField txt
instance FromField Reference where
  fromField fld m = Reference <$> fromField fld m


data PickerField = PickerField (Maybe Text) deriving Typeable
instance FromJSON PickerField where
  parseJSON fld = PickerField <$> parseJSON fld
instance ToJSON PickerField where
  toJSON (PickerField txt) = toJSON txt
instance ToField PickerField where
  toField (PickerField txt) = toField txt
instance FromField PickerField where
  fromField fld m = PickerField <$> fromField fld m


data MapField = MapField (Maybe Text) deriving Typeable
instance FromJSON MapField where
  parseJSON fld = MapField <$> parseJSON fld
instance ToJSON MapField where
  toJSON (MapField txt) = toJSON txt
instance ToField MapField where
  toField (MapField txt) = toField txt
instance FromField MapField where
  fromField fld m = MapField <$> fromField fld m


data Checkbox = Checkbox Bool deriving Typeable
instance FromJSON Checkbox where
  parseJSON fld =
    (Checkbox . (==("1"::Text)) <$> parseJSON fld) <|>
    (Checkbox . (== True)       <$> parseJSON fld)
instance ToJSON Checkbox where
  toJSON (Checkbox b) = toJSON (if b then "1" else "0" :: Text)
instance ToField Checkbox where
  toField (Checkbox b) = toField b
instance FromField Checkbox where
  fromField fld m = Checkbox <$> fromField fld m

on :: Checkbox
on = Checkbox True

off :: Checkbox
off = Checkbox False
