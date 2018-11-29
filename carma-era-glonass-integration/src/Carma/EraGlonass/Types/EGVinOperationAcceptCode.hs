{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, LambdaCase #-}
{-# LANGUAGE DeriveGeneric, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, InstanceSigs #-}

-- | Type of the "acceptCode" field for Call Card operations requests.
module Carma.EraGlonass.Types.EGVinOperationAcceptCode
     ( EGVinOperationAcceptCode (..)
     , EGVinOperationFailureAcceptCode (..)
     , EGVinOperationDeletionIsSucceededAcceptCode (..)
     ) where

import           GHC.Generics (Generic)

import           Data.Proxy
import           Data.Aeson
import           Data.Aeson.Types (Parser, typeMismatch)
import           Data.Swagger

import           Carma.Utils.StringyEnum
import           Carma.Utils.StringyEnum.SwaggerSchema
import           Carma.Utils.TypeSafe.Generic.DataType


data EGVinOperationAcceptCode
   = OK
   | IncorrectFormat
   | VinNotFound
     deriving (Eq, Enum, Bounded, Show, Generic)

instance StringyEnum EGVinOperationAcceptCode where
  toStringy = \case
    OK              -> "OK"
    IncorrectFormat -> "INCORRECT_FORMAT"
    VinNotFound     -> "VIN_NOT_FOUND"

instance FromJSON EGVinOperationAcceptCode where
  -- | Producing list of all values to reduce human-factor mistakes,
  -- so it is handled automatically when we add a new value.
  --
  -- Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  parseJSON :: forall t. t ~ EGVinOperationAcceptCode => Value -> Parser t
  parseJSON jsonValue = f [minBound..maxBound :: t]
    where f [] = typeMismatch (typeName (Proxy :: Proxy t)) jsonValue
          f (x:xs) | toJSON x == jsonValue = pure x
                   | otherwise             = f xs

instance ToJSON EGVinOperationAcceptCode where
  toJSON = String . toStringy

instance ToSchema EGVinOperationAcceptCode where
  declareNamedSchema = stringyEnumNamedSchema


-- | An additional type for "EGVinOperationAcceptCode" when @OK@ is excluded.
--
-- For failure cases when @acceptCode@ could never be @OK@.
data EGVinOperationFailureAcceptCode
   = FailureIncorrectFormat
   | FailureVinNotFound
     deriving (Eq, Enum, Bounded, Show, Generic)

instance StringyEnum EGVinOperationFailureAcceptCode where
  toStringy = \case
    FailureIncorrectFormat -> toStringy IncorrectFormat
    FailureVinNotFound     -> toStringy VinNotFound

instance FromJSON EGVinOperationFailureAcceptCode where
  -- | Producing list of all values to reduce human-factor mistakes,
  -- so it is handled automatically when we add a new value.
  --
  -- Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  parseJSON
    :: forall t. t ~ EGVinOperationFailureAcceptCode => Value -> Parser t

  parseJSON jsonValue =
    f [minBound..maxBound :: t]
    where f [] = typeMismatch (typeName (Proxy :: Proxy t)) jsonValue
          f (x:xs) | toJSON x == jsonValue = pure x
                   | otherwise             = f xs

instance ToJSON EGVinOperationFailureAcceptCode where
  toJSON = String . toStringy

instance ToSchema EGVinOperationFailureAcceptCode where
  declareNamedSchema = stringyEnumNamedSchema


-- | An additional type for "EGVinOperationAcceptCode" for DELETE operation.
--
-- For cases when @acceptCode@ is either @OK@ or @VIN_NOT_FOUND@ and these both
-- cases are means it's okay, no errors.
data EGVinOperationDeletionIsSucceededAcceptCode
   = DeletionIsSucceededOK
   | DeletionIsSucceededVinNotFound
     deriving (Eq, Enum, Bounded, Show, Generic)

instance StringyEnum EGVinOperationDeletionIsSucceededAcceptCode where
  toStringy = \case
    DeletionIsSucceededOK          -> toStringy OK
    DeletionIsSucceededVinNotFound -> toStringy VinNotFound

instance FromJSON EGVinOperationDeletionIsSucceededAcceptCode where
  -- | Producing list of all values to reduce human-factor mistakes,
  -- so it is handled automatically when we add a new value.
  --
  -- Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  parseJSON
    :: forall t. t ~ EGVinOperationDeletionIsSucceededAcceptCode
    => Value
    -> Parser t

  parseJSON jsonValue =
    f [minBound..maxBound :: t]
    where f [] = typeMismatch (typeName (Proxy :: Proxy t)) jsonValue
          f (x:xs) | toJSON x == jsonValue = pure x
                   | otherwise             = f xs

instance ToJSON EGVinOperationDeletionIsSucceededAcceptCode where
  toJSON = String . toStringy

instance ToSchema EGVinOperationDeletionIsSucceededAcceptCode where
  declareNamedSchema = stringyEnumNamedSchema
