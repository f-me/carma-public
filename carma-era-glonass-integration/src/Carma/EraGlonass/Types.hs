{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Carma.EraGlonass.Types
     ( AppContext (..)
     , EraGlonassCreateCallCardRequest (..)
     ) where

import           GHC.Generics (Generic)

import           Data.Int
import           Data.Word
import           Data.Text
import           Data.Swagger.Schema (ToSchema)
import           Data.Aeson (FromJSON)

import           Control.Concurrent.MVar (MVar)

import           Carma.Monad.LoggerBus.Types (LogMessage)
import           Carma.EraGlonass.RequestId (RequestId)


data AppContext
   = AppContext
   { -- A bus to send log messages to
     loggerBus :: MVar LogMessage
   }


data EraGlonassCreateCallCardRequest
   = EraGlonassCreateCallCardRequest
   { requestId :: RequestId
       -- ^ Unique request identifier (required to answer)
       --   CaRMa field: TODO

   , cardIdCC :: Word64
       -- ^ Identity of "Call Card" (required to answer)
       --   CaRMa field: TODO

   , atPhoneNumber :: Text
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
   } deriving (Generic, ToSchema, FromJSON)
