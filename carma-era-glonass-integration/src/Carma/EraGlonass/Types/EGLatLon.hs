{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, TypeFamilies, InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}

module Carma.EraGlonass.Types.EGLatLon
     ( EGLatitude, toEGLatitude, fromEGLatitude
     , EGLongitude, toEGLongitude, fromEGLongitude
     ) where

import           GHC.Generics

import           Data.Proxy
import           Data.Int
import           Data.String (IsString (fromString))
import           Data.Aeson
import           Data.Aeson.Types (Parser, typeMismatch)
import           Data.Swagger
import           Data.Swagger.Declare (Declare)
import           Text.InterpolatedString.QM

import           Carma.Utils.TypeSafe.Generic.DataType


newtype EGLatitude = EGLatitude Int32 deriving (Show, Eq, Ord, Generic)

fromEGLatitude :: EGLatitude -> Int32
fromEGLatitude (EGLatitude x) = x

toEGLatitude :: forall t e. (t ~ EGLatitude, IsString e) => Int32 -> Either e t
toEGLatitude x = go where
  go | x >= min' && x <= max' = Right $ EGLatitude x
     | otherwise = Left $ fromString
         [qm| A value {x} is out of {typeName''}'s bounds: {min'}..{max'} |]

  typeName'' = typeName (Proxy :: Proxy t) :: String
  min' = fromEGLatitude (minBound :: t)
  max' = fromEGLatitude (maxBound :: t)

instance Bounded EGLatitude where
  minBound = EGLatitude (-648000000)
  maxBound = EGLatitude   648000000

instance FromJSON EGLatitude where
  -- | Type annotation added here to provide type-variable @t@ inside
  --   (for type-safety reasons).
  parseJSON :: forall t. t ~ EGLatitude => Value -> Parser t
  parseJSON (Number x) =
    either fail pure $ toEGLatitude $ fst $ properFraction x
  parseJSON x = typeMismatch (typeName (Proxy :: Proxy t)) x

instance ToJSON EGLatitude where
  toJSON (EGLatitude x) = toJSON x

instance ToSchema EGLatitude where
  -- | Type annotation added here to provide type-variable @t@ inside
  --   (for type-safety reasons).
  declareNamedSchema
    :: forall proxy t. t ~ EGLatitude
    => proxy t
    -> Declare (Definitions Schema) NamedSchema

  declareNamedSchema _ = pure go where
    typeName'' = typeName (Proxy :: Proxy t)

    go
      = NamedSchema (Just typeName'') mempty
      { _schemaParamSchema = mempty
          { _paramSchemaType    = Just SwaggerInteger
          , _paramSchemaMinimum = Just $ fromIntegral $ fromEGLatitude minBound
          , _paramSchemaMaximum = Just $ fromIntegral $ fromEGLatitude maxBound
          }
      }


newtype EGLongitude = EGLongitude Int32 deriving (Show, Eq, Ord, Generic)

fromEGLongitude :: EGLongitude -> Int32
fromEGLongitude (EGLongitude x) = x

toEGLongitude
  :: forall t e. (t ~ EGLongitude, IsString e) => Int32 -> Either e t

toEGLongitude x = go where
  go | x >= min' && x <= max' = Right $ EGLongitude x
     | otherwise = Left $ fromString
         [qm| A value {x} is out of {typeName''}'s bounds: {min'}..{max'} |]

  typeName'' = typeName (Proxy :: Proxy t) :: String
  min' = fromEGLongitude (minBound :: t)
  max' = fromEGLongitude (maxBound :: t)

instance Bounded EGLongitude where
  minBound = EGLongitude (-324000000)
  maxBound = EGLongitude   324000000

instance FromJSON EGLongitude where
  -- | Type annotation added here to provide type-variable @t@ inside
  --   (for type-safety reasons).
  parseJSON :: forall t. t ~ EGLongitude => Value -> Parser t
  parseJSON (Number x) =
    either fail pure $ toEGLongitude $ fst $ properFraction x
  parseJSON x = typeMismatch (typeName (Proxy :: Proxy t)) x

instance ToJSON EGLongitude where
  toJSON (EGLongitude x) = toJSON x

instance ToSchema EGLongitude where
  -- | Type annotation added here to provide type-variable @t@ inside
  --   (for type-safety reasons).
  declareNamedSchema
    :: forall proxy t. t ~ EGLongitude
    => proxy t
    -> Declare (Definitions Schema) NamedSchema

  declareNamedSchema _ = pure go where
    typeName'' = typeName (Proxy :: Proxy t)

    go
      = NamedSchema (Just typeName'') mempty
      { _schemaParamSchema = mempty
          { _paramSchemaType    = Just SwaggerInteger
          , _paramSchemaMinimum = Just $ fromIntegral $ fromEGLongitude minBound
          , _paramSchemaMaximum = Just $ fromIntegral $ fromEGLongitude maxBound
          }
      }
