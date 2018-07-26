{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Carma.EraGlonass.Types
     ( AppContext (..)
     , EraGlonassCreateCallCardRequest (..)
     , EGPhoneNumber.EGPhoneNumber (EGPhoneNumber.EGPhoneNumber)
     , EGLatLon.EGLatitude (EGLatLon.EGLatitude)
     , EGLatLon.EGLongitude (EGLatLon.EGLongitude)
     , EGCallCardId.EGCallCardId (EGCallCardId.EGCallCardId)
     , EGCallerFullName.EGCallerFullName (EGCallerFullName.EGCallerFullName)
     ) where

import           GHC.Generics (Generic)

import           Data.Text (Text)
import           Data.Aeson
import           Data.Swagger

import           Control.Concurrent.MVar (MVar)

import           Carma.Monad.LoggerBus.Types (LogMessage)
import           Carma.EraGlonass.RequestId (RequestId)
import qualified Carma.EraGlonass.Types.EGPhoneNumber as EGPhoneNumber
import qualified Carma.EraGlonass.Types.EGLatLon as EGLatLon
import qualified Carma.EraGlonass.Types.EGCallCardId as EGCallCardId
import qualified Carma.EraGlonass.Types.EGCallerFullName as EGCallerFullName


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

   , cardIdCC :: EGCallCardId.EGCallCardId
       -- ^ Identity of "Call Card" (required to answer)
       --   Just a string, documentation says it's unsigned integer but in an
       --   example it is "597b53edf0f012e5e00d8a9a", after clarifying we know
       --   it is a free string value.
       --   CaRMa field: TODO

   , atPhoneNumber :: EGPhoneNumber.EGPhoneNumber
       -- ^ A contact phone number.
       --   A string from 1 to 18 chars of numbers which could be prefixed with
       --   '+'.
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
