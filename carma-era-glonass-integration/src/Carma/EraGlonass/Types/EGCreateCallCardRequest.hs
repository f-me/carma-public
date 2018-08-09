{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Carma.EraGlonass.Types.EGCreateCallCardRequest
     ( EGCreateCallCardRequest (..)
     , EGCreateCallCardRequestGis (..)
     , EGCreateCallCardRequestVehicle (..)
     ) where

import           GHC.Generics (Generic)

import qualified Data.HashMap.Lazy as HM
import           Data.Text (Text, length)
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Swagger

import           Carma.EraGlonass.RequestId (RequestId)
import qualified Carma.EraGlonass.Types.EGPhoneNumber as EGPhoneNumber
import qualified Carma.EraGlonass.Types.EGLatLon as EGLatLon
import qualified Carma.EraGlonass.Types.EGCallCardId as EGCallCardId
import qualified Carma.EraGlonass.Types.EGCallerFullName as EGCallerFullName
import qualified Carma.EraGlonass.Types.EGVin as EGVin
import           Carma.EraGlonass.Types.EGPropulsion


data EGCreateCallCardRequest
   = EGCreateCallCardRequest
   { requestId :: RequestId
       -- ^ Unique request identifier (required to answer)
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
   } deriving (Eq, Show, Generic, ToSchema)

instance FromJSON EGCreateCallCardRequest where
  parseJSON src = do
    parsed <- genericParseJSON defaultOptions src

    -- Handling max length of "locationDescription"
    if Data.Text.length (locationDescription parsed) > 180
       then typeMismatch "EGCreateCallCardRequest.locationDescription" src
       else pure ()

    pure parsed


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


-- EG.CRM.01 response example:
-- {
--   "responseId":"177551",
--   "cardidProvider":"120010001823039",
--   "acceptId":"597b53edf0f012e5e00d8a9a",
--   "requestId":"9db7cf43-deab-4c27-a8df-74bec0b75df1",
--   "acceptCode":"OK",
--   "statusDescription":""
-- }
