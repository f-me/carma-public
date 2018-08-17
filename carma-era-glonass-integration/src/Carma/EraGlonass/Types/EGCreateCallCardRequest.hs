{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Carma.EraGlonass.Types.EGCreateCallCardRequest
     ( EGCreateCallCardRequest (..)
     , EGCreateCallCardRequestGis (..)
     , EGCreateCallCardRequestVehicle (..)
     , EGCreateCallCardResponse (..)
     ) where

import           GHC.Generics

import           Data.Proxy
import qualified Data.HashMap.Lazy as HM
import           Data.Text (Text, length)
import           Data.Aeson
import           Data.Aeson.TH (Options (omitNothingFields))
import           Data.Aeson.Types (typeMismatch, parseEither)
import           Data.Swagger
import           Data.Swagger.Internal.Schema

import           Carma.EraGlonass.Types.RequestId (RequestId)
import qualified Carma.EraGlonass.Types.EGPhoneNumber as EGPhoneNumber
import qualified Carma.EraGlonass.Types.EGLatLon as EGLatLon
import qualified Carma.EraGlonass.Types.EGCallCardId as EGCallCardId
import qualified Carma.EraGlonass.Types.EGCallerFullName as EGCallerFullName
import qualified Carma.EraGlonass.Types.EGAcceptCode as EGAcceptCode
import qualified Carma.EraGlonass.Types.EGVin as EGVin
import           Carma.EraGlonass.Types.EGPropulsion


data EGCreateCallCardRequest
   = EGCreateCallCardRequestIncorrect String Value
     -- ^ A constructor for failure case to be able to handle incorrect request
     --   in a more flexible way (log it, store incorrect request for debugging
     --   purposes, etc.).

   | EGCreateCallCardRequest
   { requestId :: RequestId
       -- ^ Unique request identifier (required to answer)
       --   We also have been told it is an UUID.
       --   Read about UUID here:
       --     https://en.wikipedia.org/wiki/Universally_unique_identifier
       --   CaRMa field: TODO

   , cardIdCC :: EGCallCardId.EGCallCardId
       -- ^ Identity of "Call Card" (required to answer)
       --   Just a string, documentation says it's unsigned integer but in an
       --   example it is "597b53edf0f012e5e00d8a9a", after clarifying we know
       --   it is a free string value.
       --   CaRMa field: TODO

   , atPhoneNumber :: EGPhoneNumber.EGPhoneNumber
       -- ^ Phone number of a car emergency terminal
       --   (embedded SIM card or something like that).
       --   A string from 1 to 18 chars of numbers
       --   which could be prefixed with "+".
       --   CaRMa field: "contact_phone2"

   , lastTrustedLatitude :: EGLatLon.EGLatitude
       -- ^ An integer in range of -648000000 .. 648000000
       --   CaRMa field: "caseAddress_coords" (alongwith "lastTrustedLongitude")

   , lastTrustedLongitude :: EGLatLon.EGLongitude
       -- ^ An integer in range of -324000000 .. 324000000
       --   CaRMa field: "caseAddress_coords" (alongwith "lastTrustedLatitude")

   , callerFullName :: EGCallerFullName.EGCallerFullName
       -- ^ A string of 50 chars.
       --   CaRMa field: "contact_name"

   , callerPhoneNumber :: EGPhoneNumber.EGPhoneNumber
       -- ^ A phone number of a customer, his personal phone number,
       --   where "atPhoneNumber" is phone number of a car emergency terminal.
       --   A string from 1 to 18 chars of numbers
       --   which could be prefixed with '+'.
       --   CaRMa field: "contact_phone1"

   , locationDescription :: Text
       -- ^ String with max length of 180 symbols.
       --   CaRMa field: "caseAddress_comment"

   , vehicle :: EGCreateCallCardRequestVehicle
   , gis :: [EGCreateCallCardRequestGis]
   } deriving (Eq, Show, Generic)

instance FromJSON EGCreateCallCardRequest where
  -- | Handling multiple constructors here.
  --   "EGCreateCallCardRequest" is default successful case
  --   but if source JSON is incorrect we just constructing
  --   "EGCreateCallCardRequestIncorrect" and taking is it as correctly parsed
  --   keeping parse failure message and original JSON data in failure
  --   constructor.
  parseJSON src = pure $
    -- Parsing here to extract parsing error message
    case parseEither (const successfulCase) src of
         Left msg -> EGCreateCallCardRequestIncorrect msg src
         Right x  -> x

    where
      successfulCase = do
        -- Extracting hash-map from JSON "Object"
        obj <- case src of
                    Object x -> pure x
                    _        -> typeMismatch "EGCreateCallCardRequest" src

        parsed <- genericParseJSON defaultOptions $
          -- Associating it with default constructor for successful case
          Object $ HM.insert "tag" (String "EGCreateCallCardRequest") obj

        -- Handling max length of "locationDescription"
        if Data.Text.length (locationDescription parsed) > 180
           then typeMismatch "EGCreateCallCardRequest.locationDescription" src
           else pure ()

        pure parsed

-- Slicing "Incorrect" constructor from Swagger spec
-- WARNING! Hack, orphan instance. It would be better to implement it somehow in
--          "ToSchema EGCreateCallCardRequest" instance, maybe using type
--          families. This is safe enough only because name of the constructor
--          is pretty unique in the whole microservice.
instance {-# OVERLAPS #-} (GToSchema g) => GToSchema
  (C1 ('MetaCons "EGCreateCallCardRequestIncorrect" 'PrefixI 'False) a :+: g)
  where
  gdeclareNamedSchema opts _ = gdeclareNamedSchema opts (Proxy :: Proxy g)

-- Generic implementation after sliced "Incorrect" constructor
instance ToSchema EGCreateCallCardRequest


data EGCreateCallCardRequestGis
   = EGCreateCallCardRequestGis
   { regionName :: Text
       --   CaRMa field: "caseAddress_address"
   , settlementName :: Text
       --   CaRMa field: "caseAddress_address"
   , streetName :: Text
       --   CaRMa field: "caseAddress_address"
   , building :: Text
       -- ^ Number of a building as a string.
       --   CaRMa field: "caseAddress_address"
   } deriving (Eq, Show, Generic, ToSchema, FromJSON)


data EGCreateCallCardRequestVehicle
   = EGCreateCallCardRequestVehicle
   { vin :: EGVin.EGVin
       -- ^ A car's VIN ("Alphanumeric")
       --   We expect it to have length of 17 alphanumeric chars
       --   excluding "I", "O" and "Q" as declared in VIN spec.
       --   See also:
       --     https://en.wikipedia.org/wiki/Vehicle_identification_number
       --   CaRMa field: "contractIdentifier" and "car_vin" (just duplicate),
       --                see comments for these fields in "Case" model for
       --                details.

   , propulsion :: Maybe EGPropulsion
       -- ^ Enum of:
       --     "HYDROGEN"
       --     "ELECTRICITY"
       --     "LPG" - Liquefied Petroleum Gas (propane)
       --     "LNG" - Liquefied Natural Gas
       --     "DIESEL"
       --     "GASOLINE"
       --   Looking at the example I realized this field is optional
       --   (since empty string is presented for "propulsion",
       --   so I realized it is possible, and empty string is taken
       --   as "Nothing").
       --   CaRMa field: "car_engine"

   , color :: Text
       -- ^ A color of a car (string with max length of 50 symbols).
       --   CaRMa field: "car_color"

   , registrationNumber :: Text
       --   CaRMa field: "car_plateNum"
   } deriving (Eq, Show, Generic, ToSchema)

instance FromJSON EGCreateCallCardRequestVehicle where
  parseJSON src@(Object rootObj) = do
    parsed <-
      genericParseJSON defaultOptions $
        -- Interpreting empty string of "propulsion" as "Nothing"
        case HM.lookup "propulsion" rootObj of
             Just (String "") -> Object $ HM.delete "propulsion" rootObj
             _ -> src

    -- Handling max length of "color"
    if Data.Text.length (color parsed) > 50
       then typeMismatch "EGCreateCallCardRequestVehicle.color" src
       else pure ()

    pure parsed

  parseJSON x = typeMismatch "EGCreateCallCardRequestVehicle" x


data EGCreateCallCardResponse
   = EGCreateCallCardResponse
   { responseId :: Text
       -- ^ We're supposed to form it by ourselves.
       --   I think it could be just random string.

   , cardidProvider :: Text
       -- ^ CaRMa "Case" id.

   , acceptId :: Text
       -- ^ It supposed to be equal to "cardIdCC" obtained from
       --   "EGCreateCallCardRequest",
       --   just getting it from request and putting it here.

   , requestId :: RequestId
       -- ^ It supposed to be equal to "requestId" obtained from
       --   "EGCreateCallCardRequest",
       --   just getting it from request and putting it here.

   , acceptCode :: EGAcceptCode.EGAcceptCode
       -- ^ "OK"               - If data successfully accepted
       --   "INCORRECT_FORMAT" - If accepted data is incorrect
       --   "INTERNAL_ERROR"   - Internal CaRMa-side error

   , statusDescription :: Maybe Text
       -- ^ It's a free string, just a meta information, could be "OK" or an
       --   error's stack trace which would help to debug stuff.

   } deriving (Eq, Show, Generic, ToSchema)

instance ToJSON EGCreateCallCardResponse where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }
