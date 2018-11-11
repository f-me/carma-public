{-# LANGUAGE OverloadedStrings, DeriveGeneric, LambdaCase #-}

-- | Type of the "acceptCode" field for Call Card operations requests.
module Carma.EraGlonass.Types.EGCreateCallCardAcceptCode
     ( EGCreateCallCardAcceptCode (..)
     ) where

import           GHC.Generics (Generic)

import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Swagger

import           Carma.EraGlonass.Types.Helpers
                   ( StringyEnum (..)
                   , stringyEnumNamedSchema
                   )


data EGCreateCallCardAcceptCode
   = OK
   | IncorrectFormat
   | InternalError
     deriving (Eq, Enum, Bounded, Show, Generic)

instance StringyEnum EGCreateCallCardAcceptCode where
  toStringy = \case
    OK              -> "OK"
    IncorrectFormat -> "INCORRECT_FORMAT"
    InternalError   -> "INTERNAL_ERROR"

instance FromJSON EGCreateCallCardAcceptCode where
  -- Producing list of all values to reduce human-factor mistakes,
  -- so it is handled automatically when we add a new value.
  parseJSON jsonValue = f [minBound..(maxBound :: EGCreateCallCardAcceptCode)]
    where f [] = typeMismatch "EGCreateCallCardAcceptCode" jsonValue
          f (x:xs) | toJSON x == jsonValue = pure x
                   | otherwise             = f xs

instance ToJSON EGCreateCallCardAcceptCode where
  toJSON = String . toStringy

instance ToSchema EGCreateCallCardAcceptCode where
  declareNamedSchema = stringyEnumNamedSchema
