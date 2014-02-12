module Carma.Model.LegacyTypes where

import Control.Applicative
import Control.Error.Util
import Data.Text
import qualified Data.Text.Read as T
import Data.Typeable
import Data.Aeson
import Database.PostgreSQL.Simple.ToField   (ToField(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.Time (parseUTCTime)
import Data.Time.Clock.POSIX
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTime, formatTime)
import System.Locale (defaultTimeLocale)
import qualified Data.ByteString.Char8 as B

data Diagnosis1 = Diagnosis1 deriving Typeable
data Diagnosis2 = Diagnosis2 deriving Typeable
data Diagnosis3 = Diagnosis3 deriving Typeable
data Diagnosis4 = Diagnosis4 deriving Typeable
data Colors = Color deriving Typeable
data Activity = Activity deriving Typeable
data RequestType = RequestType deriving Typeable
data ConsultationType = ConsultationType deriving Typeable
data DeliveryType = DeliveryType deriving Typeable
data CarClasses = CarClasses deriving Typeable
data CarMakers = CarMakers deriving Typeable
data CarModels = CarModels deriving Typeable
data CaseStatuses = CaseStatuses deriving Typeable
data VINChecked = VINChecked deriving Typeable
data ContractType = ContractType deriving Typeable
data DealerCities = DealerCities deriving Typeable
data Partner = Partner deriving Typeable
data LegalForms = LegalForms deriving Typeable
data TechTypes = TechTypes deriving Typeable
data TowerTypes = TowerTypes deriving Typeable
data TowTypes = TowTypes deriving Typeable
data WheelsBlockedCount = WheelsBlockedCount deriving Typeable
data PaymentTypes = PaymentTypes deriving Typeable
data ClientCancelReason = ClientCancelReason deriving Typeable
data UrgentServiceReason = UrgentServiceReason deriving Typeable
data Satisfaction = Satisfaction deriving Typeable
data FalseStatuses = FalseStatuses deriving Typeable
data ServiceStatuses = ServiceStatuses deriving Typeable


data Json = Json Text deriving Typeable
instance FromJSON Json where
  parseJSON fld = Json <$> parseJSON fld
instance ToJSON Json where
  toJSON (Json txt) = toJSON txt
instance ToField Json where
  toField (Json txt) = toField txt
instance FromField Json where
  fromField fld m = Json <$> fromField fld m

data Phone = Phone Text deriving Typeable
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


data LegacyDate = LegacyDate UTCTime deriving Typeable
instance FromJSON LegacyDate where
  parseJSON fld = parseJSON fld >>= \txt ->
    case parseTime undefined "%s" (B.unpack txt) <|> hush (parseUTCTime txt) of
      Just v  -> return $ LegacyDate $ v
      Nothing -> fail $ "LegacyDatetime.parseJSON: invalid date " ++ show txt
instance ToJSON LegacyDate where
  toJSON (LegacyDate utc) = toJSON $ formatTime undefined "%s" utc
instance ToField LegacyDate where
  toField (LegacyDate utc) = toField utc
instance FromField LegacyDate where
  fromField fld m = LegacyDate <$> fromField fld m

data LegacyDatetime = LegacyDatetime UTCTime deriving Typeable
instance FromJSON LegacyDatetime where
  parseJSON fld = parseJSON fld >>= \txt ->
    case parseTime undefined "%s" (B.unpack txt) <|> hush (parseUTCTime txt) of
      Just v  -> return $ LegacyDatetime $ v
      Nothing -> fail $ "LegacyDatetime.parseJSON: invalid date " ++ show txt
instance ToJSON LegacyDatetime where
  toJSON (LegacyDatetime utc) = toJSON $ formatTime undefined "%s" utc
instance ToField LegacyDatetime where
  toField (LegacyDatetime utc) = toField utc
instance FromField LegacyDatetime where
  fromField fld m = LegacyDatetime <$> fromField fld m
