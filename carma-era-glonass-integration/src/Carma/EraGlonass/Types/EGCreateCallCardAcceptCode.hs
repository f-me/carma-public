{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, TypeFamilies, InstanceSigs #-}

-- | Type of the "acceptCode" field for Call Card operations requests.
module Carma.EraGlonass.Types.EGCreateCallCardAcceptCode
     ( EGCreateCallCardAcceptCode (..)
     ) where

import           GHC.Generics (Generic)

import           Data.Proxy
import           Data.Aeson
import           Data.Aeson.Types (Parser, typeMismatch)
import           Data.Swagger

import           Carma.EraGlonass.Types.Helpers


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
  -- | Producing list of all values to reduce human-factor mistakes,
  -- so it is handled automatically when we add a new value.
  --
  -- Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  parseJSON :: forall t. t ~ EGCreateCallCardAcceptCode => Value -> Parser t
  parseJSON jsonValue = f [minBound..(maxBound :: EGCreateCallCardAcceptCode)]
    where f [] = typeMismatch (typeName (Proxy :: Proxy t)) jsonValue
          f (x:xs) | toJSON x == jsonValue = pure x
                   | otherwise             = f xs

instance ToJSON EGCreateCallCardAcceptCode where
  toJSON = String . toStringy

instance ToSchema EGCreateCallCardAcceptCode where
  declareNamedSchema = stringyEnumNamedSchema
