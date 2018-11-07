{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric, FlexibleContexts  #-}

-- | Type of the "acceptCode" field for Call Card operations requests.
module Carma.EraGlonass.Types.EGVinOperationAcceptCode
     ( EGVinOperationAcceptCode (..)
     , EGVinOperationFailureAcceptCode (..)
     ) where

import           GHC.Generics (Generic)

import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Swagger

import           Carma.EraGlonass.Types.Helpers (stringyEnumNamedSchema)


data EGVinOperationAcceptCode
   = OK
   | IncorrectFormat
   | VinNotFound
     deriving (Eq, Enum, Bounded, Show, Generic)

instance FromJSON EGVinOperationAcceptCode where
  -- Producing list of all values to reduce human-factor mistakes,
  -- so it is handled automatically when we add a new value.
  parseJSON jsonValue = f [minBound..(maxBound :: EGVinOperationAcceptCode)]
    where f [] = typeMismatch "EGVinOperationAcceptCode" jsonValue
          f (x:xs) | toJSON x == jsonValue = pure x
                   | otherwise             = f xs

instance ToJSON EGVinOperationAcceptCode where
  toJSON OK              = String "OK"
  toJSON IncorrectFormat = String "INCORRECT_FORMAT"
  toJSON VinNotFound     = String "VIN_NOT_FOUND"

instance ToSchema EGVinOperationAcceptCode where
  declareNamedSchema = stringyEnumNamedSchema


-- | An additional type for "EGVinOperationAcceptCode" when @OK@ is excluded.
--
-- For failure cases when @acceptCode@ could never be @OK@.
data EGVinOperationFailureAcceptCode
   = FailureIncorrectFormat
   | FailureVinNotFound
     deriving (Eq, Enum, Bounded, Show, Generic)

instance FromJSON EGVinOperationFailureAcceptCode where
  -- Producing list of all values to reduce human-factor mistakes,
  -- so it is handled automatically when we add a new value.
  parseJSON jsonValue =
    f [minBound..(maxBound :: EGVinOperationFailureAcceptCode)]
    where f [] = typeMismatch "EGVinOperationFailureAcceptCode" jsonValue
          f (x:xs) | toJSON x == jsonValue = pure x
                   | otherwise             = f xs

instance ToJSON EGVinOperationFailureAcceptCode where
  toJSON FailureIncorrectFormat = toJSON IncorrectFormat
  toJSON FailureVinNotFound     = toJSON VinNotFound

instance ToSchema EGVinOperationFailureAcceptCode where
  declareNamedSchema = stringyEnumNamedSchema
