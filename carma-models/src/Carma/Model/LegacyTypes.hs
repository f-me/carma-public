
module Carma.Model.LegacyTypes where

import Control.Applicative
import Data.Text
import qualified Data.Text.Read as T
import Data.Typeable
import Data.Aeson
import Database.PostgreSQL.Simple.ToField   (ToField(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Data.Time.Clock.POSIX
import Data.Time.Clock (UTCTime)

import Data.Model.Types

data Diagnosis1 = Diagnosis1 deriving Typeable
data Diagnosis2 = Diagnosis2 deriving Typeable
data Diagnosis3 = Diagnosis3 deriving Typeable
data Diagnosis4 = Diagnosis4 deriving Typeable
data Color = Color deriving Typeable


data Reference = Reference Text deriving Typeable
instance FromJSON Reference where
  parseJSON fld = Reference <$> parseJSON fld
instance ToJSON Reference where
  toJSON (Reference txt) = toJSON txt
instance ToField Reference where
  toField (Reference txt) = toField txt
instance FromField Reference where
  fromField fld m = Reference <$> fromField fld m


data PickerField = PickerField Text deriving Typeable
instance FromJSON PickerField where
  parseJSON fld = PickerField <$> parseJSON fld
instance ToJSON PickerField where
  toJSON (PickerField txt) = toJSON txt
instance ToField PickerField where
  toField (PickerField txt) = toField txt
instance FromField PickerField where
  fromField fld m = PickerField <$> fromField fld m


data MapField = MapField Text deriving Typeable
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
  parseJSON fld = Checkbox . (==("1"::Text)) <$> parseJSON fld
instance ToJSON Checkbox where
  toJSON (Checkbox b) = toJSON (if b then "1" else "0" :: Text)
instance ToField Checkbox where
  toField (Checkbox b) = toField b
instance FromField Checkbox where
  fromField fld m = Checkbox <$> fromField fld m


data LegacyDate = LegacyDate UTCTime deriving Typeable
instance FromJSON LegacyDate where
  parseJSON fld = parseJSON fld >>= \txt -> case T.decimal txt of
    Right (res, "") -> return $ LegacyDate $ posixSecondsToUTCTime (fromInteger res)
    _ -> fail $ "LegacyDate.parseJSON: invalid date " ++ show txt
instance ToJSON LegacyDate where
  toJSON (LegacyDate utc) = toJSON $ show $ utcTimeToPOSIXSeconds utc
instance ToField LegacyDate where
  toField (LegacyDate utc) = toField utc
instance FromField LegacyDate where
  fromField fld m = LegacyDate <$> fromField fld m
