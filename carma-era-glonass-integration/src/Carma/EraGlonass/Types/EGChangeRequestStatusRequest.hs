{-# LANGUAGE DeriveGeneric, DeriveAnyClass, ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, InstanceSigs #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts, NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- Fixes issue when record-fields aren't exported. Probably related to:
--   https://stackoverflow.com/questions/46357747/haddock-data-record-fields-names-not-being-generated
{-# OPTIONS_HADDOCK ignore-exports #-}

module Carma.EraGlonass.Types.EGChangeRequestStatusRequest
     ( EGChangeRequestStatusRequest           (..)
     , EGChangeRequestStatusRequestStatusCode (..)

     , proof
     ) where

import           GHC.Generics

import           Data.Proxy
import           Data.Aeson
import           Data.Swagger

import           Carma.Utils.Operators
import           Carma.Utils.StringyEnum
import           Carma.Utils.StringyEnum.Aeson
import           Carma.Utils.StringyEnum.SwaggerSchema
import           Carma.EraGlonass.Types.Helpers.Proof
import           Carma.EraGlonass.Types.Helpers.NonEmptyText
import           Carma.EraGlonass.Types.EGRequestId (EGRequestId)
import           Carma.EraGlonass.Types.EGDateTime (EGDateTime)


proof :: ()
proof
  = proofThatTheTypeIsComplete (Proxy :: Proxy EGChangeRequestStatusRequest)
  ! proofThatTheTypeIsComplete
      (Proxy :: Proxy EGChangeRequestStatusRequestStatusCode)


-- | Response time for this is equal to @EGChangeProcessingStatusResponse@
--   (as we have been told by the spec, it haven't even shown duplicate scheme),
--   we're just using that type instead of duplicating it here.
data EGChangeRequestStatusRequest
   = EGChangeRequestStatusRequest
   { requestId :: EGRequestId
       -- ^ Unique identity of a request for service.

   , statusCode :: EGChangeRequestStatusRequestStatusCode
       -- ^ New status for this request for service.

   , statusTime :: EGDateTime
       -- ^ The time when this status has been changed.
       --
       -- It possibly can be earlier than current time, when we're making this
       -- request, this request is just a synchronization of that status.
       -- First we change the status in our database and then sending this
       -- request.

   , comment :: Maybe NonEmptyText
       -- ^ Optional comment left by us, our operator or something like that.

   } deriving (Eq, Show, Generic, ToJSON, ToSchema)

instance FromJSON EGChangeRequestStatusRequest where
  parseJSON =
    parseJSONWithOptionalNonEmptyTextFields'
      (Proxy :: Proxy '(t, "EGChangeRequestStatusRequest", 1))


data EGChangeRequestStatusRequestStatusCode
   = WorkInProgress
   | Done
   | ClientDenial
   | ServiceUnsupported
     deriving (Eq, Show, Enum, Bounded, Generic)

instance StringyEnum EGChangeRequestStatusRequestStatusCode where
  toStringy WorkInProgress     = "WORK_IN_PROGRESS"
  toStringy Done               = "DONE"
  toStringy ClientDenial       = "CLIENT_DENIAL"
  toStringy ServiceUnsupported = "SERVICE_UNSUPPORTED"

instance ToJSON EGChangeRequestStatusRequestStatusCode where
  toJSON = String . toStringy

instance FromJSON EGChangeRequestStatusRequestStatusCode where
  parseJSON = parseStringyEnumJSON

instance ToSchema EGChangeRequestStatusRequestStatusCode where
  declareNamedSchema = stringyEnumNamedSchema
