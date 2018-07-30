{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Carma.EraGlonass.Types.EGLatLon
     ( EGLatitude, toEGLatitude, fromEGLatitude
     , EGLongitude, toEGLongitude, fromEGLongitude
     ) where

import           Data.Int
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Swagger
import           Text.InterpolatedString.QM


newtype EGLatitude
      = EGLatitude { fromEGLatitude :: Int32 }
        deriving (Show, Eq)

toEGLatitude :: Int32 -> EGLatitude
toEGLatitude x
  | x >= fromEGLatitude minBound &&
    x <= fromEGLatitude maxBound = EGLatitude x
  | otherwise = error [qms|
      A value {x} is out of EGLatitude's bounds:
      {fromEGLatitude minBound}..{fromEGLatitude maxBound}
    |]

instance Bounded EGLatitude where
  minBound = EGLatitude (-648000000)
  maxBound = EGLatitude   648000000

instance FromJSON EGLatitude where
  parseJSON j@(Number x)
    | x >= fromIntegral (fromEGLatitude minBound) &&
      x <= fromIntegral (fromEGLatitude maxBound) =
        pure $ toEGLatitude $ fst $ properFraction x
    | otherwise = typeMismatch "EGLatitude" j
  parseJSON x = typeMismatch "EGLatitude" x

instance ToSchema EGLatitude where
  declareNamedSchema _ = pure $ NamedSchema (Just "EGLatitude") mempty
    { _schemaParamSchema = mempty
        { _paramSchemaType    = SwaggerInteger
        , _paramSchemaMinimum = Just $ fromIntegral $ fromEGLatitude minBound
        , _paramSchemaMaximum = Just $ fromIntegral $ fromEGLatitude maxBound
        }
    }


newtype EGLongitude
      = EGLongitude { fromEGLongitude :: Int32 }
        deriving (Show, Eq)

toEGLongitude :: Int32 -> EGLongitude
toEGLongitude x
  | x >= fromEGLongitude minBound &&
    x <= fromEGLongitude maxBound = EGLongitude x
  | otherwise = error [qms|
      A value {x} is out of EGLongitude's bounds:
      {fromEGLongitude minBound}..{fromEGLongitude maxBound}
    |]

instance Bounded EGLongitude where
  minBound = EGLongitude (-324000000)
  maxBound = EGLongitude   324000000

instance FromJSON EGLongitude where
  parseJSON j@(Number x)
    | x >= fromIntegral (fromEGLongitude minBound) &&
      x <= fromIntegral (fromEGLongitude maxBound) =
        pure $ toEGLongitude $ fst $ properFraction x
    | otherwise = typeMismatch "EGLongitude" j
  parseJSON x = typeMismatch "EGLongitude" x

instance ToSchema EGLongitude where
  declareNamedSchema _ = pure $ NamedSchema (Just "EGLongitude") mempty
    { _schemaParamSchema = mempty
        { _paramSchemaType    = SwaggerInteger
        , _paramSchemaMinimum = Just $ fromIntegral $ fromEGLongitude minBound
        , _paramSchemaMaximum = Just $ fromIntegral $ fromEGLongitude maxBound
        }
    }
