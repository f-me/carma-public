{-# LANGUAGE OverloadedStrings, DeriveGeneric, LambdaCase #-}

-- | Type of the "acceptCode" field for Call Card operations requests.
module Carma.EraGlonass.Types.EGUpdateCallCardAcceptCode
     ( EGUpdateCallCardAcceptCode (..)
     ) where

import           GHC.Generics (Generic)

import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Swagger

import           Carma.EraGlonass.Types.Helpers
                   ( StringyEnum (..)
                   , stringyEnumNamedSchema
                   )


data EGUpdateCallCardAcceptCode
   = OK
   | IncorrectFormat
   | CardNotFound
     deriving (Eq, Enum, Bounded, Show, Generic)

instance StringyEnum EGUpdateCallCardAcceptCode where
  toStringy = \case
    OK              -> "OK"
    IncorrectFormat -> "INCORRECT_FORMAT"
    CardNotFound    -> "CARD_NOT_FOUND"

instance FromJSON EGUpdateCallCardAcceptCode where
  -- Producing list of all values to reduce human-factor mistakes,
  -- so it is handled automatically when we add a new value.
  parseJSON jsonValue = f [minBound..(maxBound :: EGUpdateCallCardAcceptCode)]
    where f [] = typeMismatch "EGUpdateCallCardAcceptCode" jsonValue
          f (x:xs) | toJSON x == jsonValue = pure x
                   | otherwise             = f xs

instance ToJSON EGUpdateCallCardAcceptCode where
  toJSON = String . toStringy

instance ToSchema EGUpdateCallCardAcceptCode where
  declareNamedSchema = stringyEnumNamedSchema
