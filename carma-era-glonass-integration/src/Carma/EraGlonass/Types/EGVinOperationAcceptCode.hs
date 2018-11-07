{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric, FlexibleContexts  #-}
{-# LANGUAGE QuasiQuotes, LambdaCase #-}

-- | Type of the "acceptCode" field for Call Card operations requests.
module Carma.EraGlonass.Types.EGVinOperationAcceptCode
     ( EGVinOperationAcceptCode (..)
     , EGVinOperationFailureAcceptCode (..)
     ) where

import           GHC.Generics (Generic, Rep)

import           Text.InterpolatedString.QM
import           Data.Proxy
import           Data.Swagger.Declare
import           Data.Swagger.Internal.Schema
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Swagger

import           Carma.Utils.Operators


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


stringyEnumNamedSchema
  :: (GToSchema (Rep a), ToJSON a, Enum a, Bounded a)
  => proxy a
  -> Declare (Definitions Schema) NamedSchema

stringyEnumNamedSchema p = do
  NamedSchema name' schema' <-
    gdeclareNamedSchema defaultSchemaOptions (repProxy p) mempty

  pure $ NamedSchema name' schema'
    { _schemaParamSchema = (_schemaParamSchema schema')
        { _paramSchemaType = SwaggerString
        , _paramSchemaEnum = Just $ wholeEnum p
        }
    }

  where
    repProxy :: proxy p -> Proxy (Rep p)
    repProxy _ = Proxy

    wholeEnum :: (ToJSON a, Enum a, Bounded a) => proxy a -> [Value]
    wholeEnum p' =
       enum' p' <&> toJSON <&> \case
         y@(String _) -> y
         _ -> error [qms| "stringyEnumNamedSchema":
                          Every constructor must be resolved to
                          "String" by using "toJSON" |]

      where enum' :: (Enum a, Bounded a) => proxy a -> [a]
            enum' _ = [minBound..maxBound]
