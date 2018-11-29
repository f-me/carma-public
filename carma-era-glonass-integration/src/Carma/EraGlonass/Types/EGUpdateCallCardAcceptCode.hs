{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, TypeFamilies, InstanceSigs #-}

-- | Type of the "acceptCode" field for Call Card operations requests.
module Carma.EraGlonass.Types.EGUpdateCallCardAcceptCode
     ( EGUpdateCallCardAcceptCode (..)
     ) where

import           GHC.Generics (Generic)

import           Data.Proxy
import           Data.Aeson
import           Data.Aeson.Types (Parser, typeMismatch)
import           Data.Swagger

import           Carma.Utils.StringyEnum
import           Carma.Utils.StringyEnum.SwaggerSchema
import           Carma.EraGlonass.Types.Helpers


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
  -- | Producing list of all values to reduce human-factor mistakes,
  -- so it is handled automatically when we add a new value.
  --
  -- Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  parseJSON :: forall t. t ~ EGUpdateCallCardAcceptCode => Value -> Parser t
  parseJSON jsonValue = f [minBound..(maxBound :: EGUpdateCallCardAcceptCode)]
    where f [] = typeMismatch (typeName (Proxy :: Proxy t)) jsonValue
          f (x:xs) | toJSON x == jsonValue = pure x
                   | otherwise             = f xs

instance ToJSON EGUpdateCallCardAcceptCode where
  toJSON = String . toStringy

instance ToSchema EGUpdateCallCardAcceptCode where
  declareNamedSchema = stringyEnumNamedSchema
