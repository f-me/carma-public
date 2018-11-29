{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, TypeFamilies, InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}

module Carma.EraGlonass.Types.EGLatLon
     ( EGLatitude, toEGLatitude, fromEGLatitude
     , EGLongitude, toEGLongitude, fromEGLongitude
     ) where

import           GHC.Generics

import           Data.Proxy
import           Data.Int
import           Data.Aeson
import           Data.Aeson.Types (Parser, typeMismatch)
import           Data.Swagger
import           Data.Swagger.Declare (Declare)
import           Text.InterpolatedString.QM

import           Carma.Utils.TypeSafe.Generic.DataType


newtype EGLatitude
      = EGLatitude { fromEGLatitude :: Int32 }
        deriving (Show, Eq, Generic)

toEGLatitude :: forall t. t ~ EGLatitude => Int32 -> t
toEGLatitude x
  | x >= fromEGLatitude minBound &&
    x <= fromEGLatitude maxBound = EGLatitude x
  | otherwise = error [qms|
      A value {x} is out of {typeName (Proxy :: Proxy t) :: String}'s bounds:
      {fromEGLatitude minBound}..{fromEGLatitude maxBound}
    |]

instance Bounded EGLatitude where
  minBound = EGLatitude (-648000000)
  maxBound = EGLatitude   648000000

instance FromJSON EGLatitude where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  parseJSON :: forall t. t ~ EGLatitude => Value -> Parser t
  parseJSON j@(Number x)
    | x >= fromIntegral (fromEGLatitude minBound) &&
      x <= fromIntegral (fromEGLatitude maxBound) =
        pure $ toEGLatitude $ fst $ properFraction x
    | otherwise = typeMismatch (typeName (Proxy :: Proxy t)) j
  parseJSON x = typeMismatch (typeName (Proxy :: Proxy t)) x

instance ToJSON EGLatitude where
  toJSON (EGLatitude x) = toJSON x

instance ToSchema EGLatitude where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  declareNamedSchema
    :: forall proxy t. (t ~ EGLatitude)
    => proxy t
    -> Declare (Definitions Schema) NamedSchema

  declareNamedSchema _ = pure $ NamedSchema (Just typeName'') mempty
    { _schemaParamSchema = mempty
        { _paramSchemaType    = SwaggerInteger
        , _paramSchemaMinimum = Just $ fromIntegral $ fromEGLatitude minBound
        , _paramSchemaMaximum = Just $ fromIntegral $ fromEGLatitude maxBound
        }
    }
    where typeName'' = typeName (Proxy :: Proxy t)


newtype EGLongitude
      = EGLongitude { fromEGLongitude :: Int32 }
        deriving (Show, Eq, Generic)

toEGLongitude :: forall t. t ~ EGLongitude => Int32 -> t
toEGLongitude x
  | x >= fromEGLongitude minBound &&
    x <= fromEGLongitude maxBound = EGLongitude x
  | otherwise = error [qms|
      A value {x} is out of {typeName (Proxy :: Proxy t) :: String}'s bounds:
      {fromEGLongitude minBound}..{fromEGLongitude maxBound}
    |]

instance Bounded EGLongitude where
  minBound = EGLongitude (-324000000)
  maxBound = EGLongitude   324000000

instance FromJSON EGLongitude where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  parseJSON :: forall t. t ~ EGLongitude => Value -> Parser t
  parseJSON j@(Number x)
    | x >= fromIntegral (fromEGLongitude minBound) &&
      x <= fromIntegral (fromEGLongitude maxBound) =
        pure $ toEGLongitude $ fst $ properFraction x
    | otherwise = typeMismatch (typeName (Proxy :: Proxy t)) j
  parseJSON x = typeMismatch (typeName (Proxy :: Proxy t)) x

instance ToJSON EGLongitude where
  toJSON (EGLongitude x) = toJSON x

instance ToSchema EGLongitude where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  declareNamedSchema
    :: forall proxy t. (t ~ EGLongitude)
    => proxy t
    -> Declare (Definitions Schema) NamedSchema

  declareNamedSchema _ = pure $ NamedSchema (Just typeName'') mempty
    { _schemaParamSchema = mempty
        { _paramSchemaType    = SwaggerInteger
        , _paramSchemaMinimum = Just $ fromIntegral $ fromEGLongitude minBound
        , _paramSchemaMaximum = Just $ fromIntegral $ fromEGLongitude maxBound
        }
    }
    where typeName'' = typeName (Proxy :: Proxy t)
