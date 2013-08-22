
module Carma.Model.Types where

import Data.Aeson
import Data.String
import Data.Time.Calendar (Day)


instance FromJSON Day where
  parseJSON v = undefined

instance ToJSON Day where
  toJSON = String . fromString . show
