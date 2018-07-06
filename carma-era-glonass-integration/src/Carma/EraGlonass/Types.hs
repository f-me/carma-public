{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Carma.EraGlonass.Types
     ( AppContext (..)
     , EraGlonassCreateCallCardRequest (..)
     , EGPhoneNumber (EGPhoneNumber)
     , EGLatitude (EGLatitude)
     , EGLongitude (EGLongitude)
     ) where

import           GHC.Generics (Generic)

import           Data.Int
import           Data.Word
import           Data.Maybe
import           Data.String (fromString)
import           Data.Text hiding (count)
import           Data.Text.Encoding (encodeUtf8)
import           Text.InterpolatedString.QM
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Swagger
import           Data.Attoparsec.ByteString.Char8

import           Control.Concurrent.MVar (MVar)
import           Control.Applicative ((<|>))

import           Carma.Monad.LoggerBus.Types (LogMessage)
import           Carma.EraGlonass.RequestId (RequestId)


data AppContext
   = AppContext
   { -- A bus to send log messages to
     loggerBus :: MVar LogMessage
   }


-- TODO restructure looking at JSON example from EG documentation
data EraGlonassCreateCallCardRequest
   = EraGlonassCreateCallCardRequest
   { requestId :: RequestId
       -- ^ Unique request identifier (required to answer)
       --   CaRMa field: TODO

   , cardIdCC :: Word64
       -- ^ Identity of "Call Card" (required to answer)
       --   CaRMa field: TODO

   , atPhoneNumber :: EGPhoneNumber
       -- ^ A contact phone number.
       --   A string from 1 to 18 chars of numbers which could be prefixed with
       --   '+'.
       --   CaRMa field: "contact_phone2"

   , lastTrustedLatitude :: EGLatitude
       -- ^ An integer in range of -648000000 .. 648000000
       --   CaRMa field: "caseAddress_coords" (alongwith "lastTrustedLongitude")

   , lastTrustedLongitude :: EGLongitude
       -- ^ An integer in range of -324000000 .. 324000000
       --   CaRMa field: "caseAddress_coords" (alongwith "lastTrustedLatitude")

   , callerFullName :: Text
       -- ^ A string of 50 chars.
       --   CaRMa field: "contact_name"

   , callerPhoneNumber :: EGPhoneNumber
       -- ^ A phone number of a customer (where "atPhoneNumber" could be some
       --   EG operator's phone I believe, TODO need to clarify this).
       --   A string from 1 to 18 chars of numbers which could be prefixed with
       --   '+'.
       --   CaRMa field: "contact_phone1"

   , locationDescription :: Text
       -- ^ String with max length of 180 symbols.
       --   CaRMa field: "caseAddress_comment"

   , vin :: Text
       -- ^ A car's VIN ("Alphanumeric")
       --   CaRMa field: "car_vin"
       --   TODO need to ask why "contractIdentifier" is proposed alongwith
       --        "car_vin".

   , propulsion :: Text
       -- ^ Enum of:
       --     "HYDROGEN"
       --     "ELECTRICITY"
       --     "LPG" - Liquefied Petroleum Gas (propane)
       --     "LNG" - Liquefied Natural Gas
       --     "DIESEL"
       --     "GASOLINE"
       --   CaRMa field: "car_engine"
       --   TODO extend "Engine" model with missed engine types

   , color :: Text
       -- ^ A color of a car (string with max length of 50 symbols).
       --   CaRMa field: "car_color"

   , registrationNumber :: Text
       --   CaRMa field: "car_plateNum"

   , regionName :: Text
       --   CaRMa field: "caseAddress_address"
   , settlementName :: Text
       --   CaRMa field: "caseAddress_address"
   , streetName :: Text
       --   CaRMa field: "caseAddress_address"
   , building :: Text
       -- ^ Number of a building as a string.
       --   CaRMa field: "caseAddress_address"

   } deriving (Generic, ToSchema, FromJSON)


-- A string from 1 to 18 chars of numbers which could be prefixed with '+'
newtype EGPhoneNumber = EGPhoneNumber Text deriving (Show, Eq)

instance FromJSON EGPhoneNumber where
  parseJSON j@(String x) =
    case parseOnly parser (encodeUtf8 x) of
         Left  _ -> typeMismatch "EGPhoneNumber" j
         Right y -> pure y

    where -- Optional plus prefix (1 char)
          --   + 1 required digit
          --   + 16 optional digits
          --   = 18 chars
          -- Maximum amount of digits is 17 where 18th char is just optional
          -- plus prefix.
          parser = f
            <$> optionalPlus
            <*> digit
            <*> count 16 optionalDigit
            <*  endOfInput
            where optionalPlus  = (Just <$> char '+') <|> pure Nothing
                  optionalDigit = (Just <$> digit)    <|> pure Nothing

                  f plus a b = EGPhoneNumber
                             $ fromString
                             $ maybe id (:) plus
                             $ a : catMaybes b

  parseJSON x = typeMismatch "EGPhoneNumber" x

instance ToSchema EGPhoneNumber where
  declareNamedSchema _ = pure
    $ NamedSchema (Just "EGPhoneNumber") mempty
    { _schemaParamSchema = mempty
        { _paramSchemaType    = SwaggerString
        , _paramSchemaFormat  = Just "phone"
        , _paramSchemaPattern = Just [qn| ^\+?[0-9]{1,17}$ |]
        }
    }


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
