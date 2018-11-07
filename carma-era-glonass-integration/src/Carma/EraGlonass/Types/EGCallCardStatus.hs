{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Carma.EraGlonass.Types.EGCallCardStatus
     ( EGCallCardStatus (..)
     ) where

import           GHC.Generics (Generic)

import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Swagger

import           Carma.EraGlonass.Types.Helpers (stringyEnumNamedSchema)


data EGCallCardStatus
   = WorkInProgress
   | Done
   | ClientDenial
   | ServiceUnsupported
     deriving (Eq, Enum, Bounded, Show, Generic)

instance FromJSON EGCallCardStatus where
  -- Producing list of all values to reduce human-factor mistakes,
  -- so it is handled automatically when we add a new value.
  parseJSON jsonValue = f [minBound..(maxBound :: EGCallCardStatus)]
    where f [] = typeMismatch "EGCallCardStatus" jsonValue
          f (x:xs) | toJSON x == jsonValue = pure x
                   | otherwise             = f xs

instance ToJSON EGCallCardStatus where
  toJSON WorkInProgress     = String "WORK_IN_PROGRESS"
  toJSON Done               = String "DONE"
  toJSON ClientDenial       = String "CLIENT_DENIAL"
  toJSON ServiceUnsupported = String "SERVICE_UNSUPPORTED"

instance ToSchema EGCallCardStatus where
  declareNamedSchema = stringyEnumNamedSchema
