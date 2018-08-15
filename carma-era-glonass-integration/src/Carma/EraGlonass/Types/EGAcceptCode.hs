{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Carma.EraGlonass.Types.EGAcceptCode
     ( EGAcceptCode (..)
     ) where

import           GHC.Generics (Generic)

import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Swagger


data EGAcceptCode
   = OK
   | IncorrectFormat
   | InternalError
     deriving (Eq, Enum, Bounded, Show, Generic, ToSchema)

instance FromJSON EGAcceptCode where
  -- Producing list of all values to reduce human-factor mistakes,
  -- so it is handled automatically when we add a new value.
  parseJSON jsonValue = f [minBound..(maxBound :: EGAcceptCode)]
    where f [] = typeMismatch "EGAcceptCode" jsonValue
          f (x:xs) | toJSON x == jsonValue = pure x
                   | otherwise             = f xs

instance ToJSON EGAcceptCode where
  toJSON OK              = String "OK"
  toJSON IncorrectFormat = String "INCORRECT_FORMAT"
  toJSON InternalError   = String "INTERNAL_ERROR"
