{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Carma.Model.Types where

import Data.Aeson
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Format (parseTime)

import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField   (ToField)
import Data.Aeson () -- (FromJSON, ToJSON)
import Data.Typeable(Typeable)
import Data.Monoid (Monoid)

import Data.Model

instance FromJSON Day where
  parseJSON (String s)
    = case parseTime undefined "%Y-%m-%d" $ T.unpack s of
      Just day -> return day
      Nothing  -> fail $ "invalid date format: " ++ show s
  parseJSON v = fail $ "invalid date: " ++ show v

instance ToJSON Day where
  toJSON = String . fromString . show

newtype Model m => Dict m = Dict Text
               deriving (FromField, ToField,
                         FromJSON, ToJSON,
                         Typeable, Monoid, IsString)

