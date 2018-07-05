{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Carma.EraGlonass.Types
     ( AppContext (..)
     , EraGlonassCreateCallCardRequest (..)
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

   , lastTrustedLatitude :: Int32
       -- ^ An integer in range of -648000000 .. 648000000
       --                Int32 is -2147483648 .. 2147483647
       --   CaRMa field: "caseAddress_coords" (alongwith "lastTrustedLongitude")

   , lastTrustedLongitude :: Int32
       -- ^ An integer in range of -324000000 .. 324000000
       --                Int32 is -2147483648 .. 2147483647
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
data EGPhoneNumber = EGPhoneNumber Text deriving (Show, Eq)

instance FromJSON EGPhoneNumber where
  -- TODO tests for parser
  parseJSON _x@(String x) =
    case parseOnly parser (encodeUtf8 x) of
         Left  _ -> typeMismatch "EGPhoneNumber" _x
         Right y -> pure y

    where parser = f
            <$> (optionalPlus *> digit)
            <*> count 16 optionalDigit
            <*  endOfInput
            where optionalPlus  = (Just <$> char '+') <|> pure Nothing
                  optionalDigit = (Just <$> digit)    <|> pure Nothing
                  f a b = EGPhoneNumber $ fromString $ a : catMaybes b

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
