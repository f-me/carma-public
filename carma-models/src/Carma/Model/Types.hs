
module Carma.Model.Types where

import Data.Aeson
import Data.String
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Format (parseTime)


instance FromJSON Day where
  parseJSON (String s)
    = case parseTime undefined "%Y-%m-%d" $ T.unpack s of
      Just day -> return day
      Nothing  -> fail $ "invalid date format: " ++ show s
  parseJSON v = fail $ "invalid date: " ++ show v

instance ToJSON Day where
  toJSON = String . fromString . show
