{-# LANGUAGE OverloadedStrings #-}

module Carma.EraGlonass.Types.EGLatLon
     ( EGLatitude (EGLatitude)
     , EGLongitude (EGLongitude)
     ) where

import           Data.Int
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Swagger


newtype EGLatitude
      = EGLatitude { fromEGLatitude :: Int32 }
        deriving (Show, Eq)

instance Bounded EGLatitude where
  minBound = EGLatitude (-648000000)
  maxBound = EGLatitude   648000000

instance FromJSON EGLatitude where
  parseJSON j@(Number x)
    | x >= fromIntegral (fromEGLatitude minBound) &&
      x <= fromIntegral (fromEGLatitude maxBound) =
        pure $ EGLatitude $ fst $ properFraction x
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

instance Bounded EGLongitude where
  minBound = EGLongitude (-324000000)
  maxBound = EGLongitude   324000000

instance FromJSON EGLongitude where
  parseJSON j@(Number x)
    | x >= fromIntegral (fromEGLongitude minBound) &&
      x <= fromIntegral (fromEGLongitude maxBound) =
        pure $ EGLongitude $ fst $ properFraction x
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
