{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

-- | Type of the "acceptCode" field for Call Card operations requests.
module Carma.EraGlonass.Types.EGUpdateCallCardAcceptCode
     ( EGUpdateCallCardAcceptCode (..)
     ) where

import           GHC.Generics (Generic)

import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Swagger

import           Carma.EraGlonass.Types.Helpers (stringyEnumNamedSchema)


data EGUpdateCallCardAcceptCode
   = OK
   | IncorrectFormat
   | CardNotFound
     deriving (Eq, Enum, Bounded, Show, Generic)

instance FromJSON EGUpdateCallCardAcceptCode where
  -- Producing list of all values to reduce human-factor mistakes,
  -- so it is handled automatically when we add a new value.
  parseJSON jsonValue = f [minBound..(maxBound :: EGUpdateCallCardAcceptCode)]
    where f [] = typeMismatch "EGUpdateCallCardAcceptCode" jsonValue
          f (x:xs) | toJSON x == jsonValue = pure x
                   | otherwise             = f xs

instance ToJSON EGUpdateCallCardAcceptCode where
  toJSON OK              = String "OK"
  toJSON IncorrectFormat = String "INCORRECT_FORMAT"
  toJSON CardNotFound    = String "CARD_NOT_FOUND"

instance ToSchema EGUpdateCallCardAcceptCode where
  declareNamedSchema = stringyEnumNamedSchema
