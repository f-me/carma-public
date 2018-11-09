{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Carma.Model.LegacyTypes where

import Data.Proxy
import Data.Typeable
import Data.Text (Text)
import Data.Aeson
import Text.InterpolatedString.QM

import Control.Applicative

import Database.PostgreSQL.Simple.ToField   (ToField (..))
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.Persist.Types (PersistValue (..), SqlType (..))
import Database.Persist.Class (PersistField (toPersistValue, fromPersistValue))
import Database.Persist.Sql (PersistFieldSql (sqlType))


newtype Password = Password Text deriving Typeable
instance Show Password where
  show _ = "Password (the password is hidden for security reasons)"
instance FromJSON Password where
  parseJSON fld = Password <$> parseJSON fld
instance ToJSON Password where
  toJSON (Password txt) = toJSON txt
instance ToField Password where
  toField (Password txt) = toField txt
instance FromField Password where
  fromField fld m = Password <$> fromField fld m


newtype Phone = Phone Text deriving (Eq, Typeable)
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
instance PersistField Phone where
  toPersistValue (Phone txt) = toPersistValue txt
  fromPersistValue (PersistText txt) = Right $ Phone txt
  fromPersistValue x = Left [qm| Expected Phone, received: {x} |]
instance PersistFieldSql Phone where
  sqlType Proxy = SqlString


newtype Reference = Reference Text deriving (Typeable, Show)
instance FromJSON Reference where
  parseJSON fld = Reference <$> parseJSON fld
instance ToJSON Reference where
  toJSON (Reference txt) = toJSON txt
instance ToField Reference where
  toField (Reference txt) = toField txt
instance FromField Reference where
  fromField fld m = Reference <$> fromField fld m
instance PersistField Reference where
  toPersistValue (Reference txt) = toPersistValue txt
  fromPersistValue (PersistText txt) = Right $ Reference txt
  fromPersistValue x = Left [qm| Expected Reference, received: {x} |]
instance PersistFieldSql Reference where
  sqlType Proxy = SqlString


newtype PickerField = PickerField (Maybe Text) deriving (Typeable, Show)
instance FromJSON PickerField where
  parseJSON fld = PickerField <$> parseJSON fld
instance ToJSON PickerField where
  toJSON (PickerField txt) = toJSON txt
instance ToField PickerField where
  toField (PickerField txt) = toField txt
instance FromField PickerField where
  fromField fld m = PickerField <$> fromField fld m
instance PersistField PickerField where
  toPersistValue (PickerField (Just txt)) = toPersistValue txt
  toPersistValue (PickerField Nothing) = PersistNull
  fromPersistValue (PersistText txt) = Right $ PickerField $ Just txt
  fromPersistValue PersistNull = Right $ PickerField Nothing
  fromPersistValue x = Left [qm| Expected PickerField, received: {x} |]
instance PersistFieldSql PickerField where
  sqlType Proxy = SqlString


newtype MapField = MapField (Maybe Text) deriving (Typeable, Show)
instance FromJSON MapField where
  parseJSON fld = MapField <$> parseJSON fld
instance ToJSON MapField where
  toJSON (MapField txt) = toJSON txt
instance ToField MapField where
  toField (MapField txt) = toField txt
instance FromField MapField where
  fromField fld m = MapField <$> fromField fld m
instance PersistField MapField where
  toPersistValue (MapField (Just txt)) = toPersistValue txt
  toPersistValue (MapField Nothing) = PersistNull
  fromPersistValue (PersistText txt) = Right $ MapField $ Just txt
  fromPersistValue PersistNull = Right $ MapField Nothing
  fromPersistValue x = Left [qm| Expected MapField, received: {x} |]
instance PersistFieldSql MapField where
  sqlType Proxy = SqlString


newtype Checkbox = Checkbox Bool deriving (Typeable, Show)
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
instance PersistField Checkbox where
  toPersistValue (Checkbox b) = toPersistValue b
  fromPersistValue (PersistBool b) = Right $ Checkbox b
  fromPersistValue x = Left [qm| Expected Checkbox, received: {x} |]
instance PersistFieldSql Checkbox where
  sqlType Proxy = SqlBool

on :: Checkbox
on = Checkbox True

off :: Checkbox
off = Checkbox False
