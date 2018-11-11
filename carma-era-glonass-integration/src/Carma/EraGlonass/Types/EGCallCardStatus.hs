{-# LANGUAGE OverloadedStrings, DeriveGeneric, LambdaCase #-}

module Carma.EraGlonass.Types.EGCallCardStatus
     ( EGCallCardStatus (..)
     ) where

import           GHC.Generics (Generic)

import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Swagger

import           Carma.EraGlonass.Types.Helpers
                   ( StringyEnum (..)
                   , stringyEnumNamedSchema
                   )


data EGCallCardStatus
   = WorkInProgress
   | Done
   | ClientDenial
   | ServiceUnsupported
     deriving (Eq, Enum, Bounded, Show, Generic)

instance StringyEnum EGCallCardStatus where
  toStringy = \case
    WorkInProgress     -> "WORK_IN_PROGRESS"
    Done               -> "DONE"
    ClientDenial       -> "CLIENT_DENIAL"
    ServiceUnsupported -> "SERVICE_UNSUPPORTED"

instance FromJSON EGCallCardStatus where
  -- Producing list of all values to reduce human-factor mistakes,
  -- so it is handled automatically when we add a new value.
  parseJSON jsonValue = f [minBound..(maxBound :: EGCallCardStatus)]
    where f [] = typeMismatch "EGCallCardStatus" jsonValue
          f (x:xs) | toJSON x == jsonValue = pure x
                   | otherwise             = f xs

instance ToJSON EGCallCardStatus where
  toJSON = String . toStringy

instance ToSchema EGCallCardStatus where
  declareNamedSchema = stringyEnumNamedSchema
