{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, TypeFamilies, InstanceSigs #-}

module Carma.EraGlonass.Types.EGCallCardStatus
     ( EGCallCardStatus (..)
     ) where

import           GHC.Generics (Generic)

import           Data.Proxy
import           Data.Aeson
import           Data.Aeson.Types (Parser, typeMismatch)
import           Data.Swagger

import           Carma.EraGlonass.Types.Helpers


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
  -- | Producing list of all values to reduce human-factor mistakes,
  -- so it is handled automatically when we add a new value.
  --
  -- Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  parseJSON :: forall t. t ~ EGCallCardStatus => Value -> Parser t
  parseJSON jsonValue = f [minBound..(maxBound :: EGCallCardStatus)]
    where f [] = typeMismatch (typeName (Proxy :: Proxy t)) jsonValue
          f (x:xs) | toJSON x == jsonValue = pure x
                   | otherwise             = f xs

instance ToJSON EGCallCardStatus where
  toJSON = String . toStringy

instance ToSchema EGCallCardStatus where
  declareNamedSchema = stringyEnumNamedSchema
