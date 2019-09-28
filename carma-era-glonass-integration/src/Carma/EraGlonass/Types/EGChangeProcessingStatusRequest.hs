{-# LANGUAGE DeriveGeneric, DeriveAnyClass, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, InstanceSigs #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts, NamedFieldPuns #-}

-- Fixes issue when record-fields aren't exported. Probably related to:
--   https://stackoverflow.com/questions/46357747/haddock-data-record-fields-names-not-being-generated
{-# OPTIONS_HADDOCK ignore-exports #-}

module Carma.EraGlonass.Types.EGChangeProcessingStatusRequest
     ( EGChangeProcessingStatusRequest (..)
     , EGChangeProcessingStatusRequestStatusCode (..)

     , EGChangeProcessingStatusResponse (..)
     , EGChangeProcessingStatusResponseFailureResultCode (..)
     , EGChangeProcessingStatusResponseError (..)
     , EGChangeProcessingStatusResponseErrorCode (..)

     , proof
     ) where

import           GHC.Generics

import           Data.Proxy
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.HashMap.Lazy as HM
import           Data.Swagger hiding (description)
import           Data.Swagger.Internal.Schema
import           Data.Swagger.Declare (Declare)

import           Carma.Utils.Operators
import           Carma.Utils.StringyEnum
import           Carma.Utils.StringyEnum.Aeson
import           Carma.Utils.StringyEnum.SwaggerSchema
import           Carma.Utils.TypeSafe.Proxy
import           Carma.Utils.TypeSafe.Generic.Aeson
import           Carma.Utils.TypeSafe.Generic.Record
import           Carma.Utils.TypeSafe.Generic.DataType.Operations.MapConstructor
import           Carma.EraGlonass.Types.Helpers.Proof
import           Carma.EraGlonass.Types.Helpers.Aeson
import           Carma.EraGlonass.Types.Helpers.NonEmptyText
import           Carma.EraGlonass.Types.EGRequestId (EGRequestId)
import           Carma.EraGlonass.Types.EGContractId (EGContractId)
import           Carma.EraGlonass.Types.EGIntegrationPoint
                   ( EGIntegrationPoint (ChangeProcessingStatus)
                   )


proof :: ()
proof
  = proofThatTheTypeIsComplete (Proxy :: Proxy EGChangeProcessingStatusRequest)
  ! proofThatTheTypeIsComplete
      (Proxy :: Proxy EGChangeProcessingStatusRequestStatusCode)

  ! proofThatTheTypeIsComplete (Proxy :: Proxy EGChangeProcessingStatusResponse)
  ! proofThatTheTypeIsComplete
      (Proxy :: Proxy EGChangeProcessingStatusResponseFailureResultCode)
  ! proofThatTheTypeIsComplete
      (Proxy :: Proxy EGChangeProcessingStatusResponseError)
  ! proofThatTheTypeIsComplete
      (Proxy :: Proxy EGChangeProcessingStatusResponseErrorCode)


data EGChangeProcessingStatusRequest
   = EGChangeProcessingStatusRequest
   { requestId :: EGRequestId
       -- ^ Unique identity of a request for service
       --   we're changing processing status of.

   , ivsPhoneNumber :: Maybe NonEmptyText
       -- ^ Phone number of a terminal integrated to a vehicle.

   , fullName :: Maybe NonEmptyText
       -- ^ Surname, first name and middle name of a user of service (caller).

   , phoneNumber :: Maybe NonEmptyText
       -- ^ Phone number of a user of service (caller).

   , statusCode :: EGChangeProcessingStatusRequestStatusCode
       -- ^ New status of a request for service.

   , serviceCategoryId :: Maybe NonEmptyText
       -- ^ This a category of service which is chosen by EG's operator.
       --
       -- We probably don't need this value for our internal purposes.
       -- We don't save this value when we're receiving a request for service so
       -- we have nothing to provide here, since this field is optional here
       -- it's not a problem.
       --
       -- Just ignore this field, it's added only to fully implement the spec.

   , serviceName :: Maybe NonEmptyText
       -- ^ A "kind of provided service".
       --
       -- I believe it's a free string, we can write anything here,
       -- or also free to leave it empty.

   , contractId :: Maybe (EGContractId 'ChangeProcessingStatus)
       -- ^ Identity of a contract of a service provider (CaRMa).
       --
       -- We've been told that we can omit this field since the service already
       -- know who are sending a request.
       --
       -- See "Carma.EraGlonass.Types.EGContractId.EGContractId" datatype for
       -- details.

   } deriving (Eq, Show, Generic, ToJSON, ToSchema)

instance FromJSON EGChangeProcessingStatusRequest where
  parseJSON =
    parseJSONWithOptionalNonEmptyTextFields'
      (Proxy :: Proxy '(t, "EGChangeProcessingStatusRequest", 5))


data EGChangeProcessingStatusRequestStatusCode
   = Closed  -- ^ Service is provided, we're done
   | Missed  -- ^ Failed to get through to client
   | Refused -- ^ Refused to deliver a service by us
     deriving (Eq, Show, Enum, Bounded, Generic)

instance StringyEnum EGChangeProcessingStatusRequestStatusCode where
  toStringy Closed  = "CLOSED"
  toStringy Missed  = "MISSED"
  toStringy Refused = "REFUSED"

instance ToJSON EGChangeProcessingStatusRequestStatusCode where
  toJSON = String . toStringy

instance FromJSON EGChangeProcessingStatusRequestStatusCode where
  parseJSON = parseStringyEnumJSON

instance ToSchema EGChangeProcessingStatusRequestStatusCode where
  declareNamedSchema = stringyEnumNamedSchema


data EGChangeProcessingStatusResponse
   -- | When @resultCode@ equals to @\"OK"@.
   --
   -- @errors@ could be empty list or I believe (but not sure) could be not
   -- set at all, so we have to add proper implementation for that in
   -- @FromJSON@ instance. We could add @Maybe@ but we already have empty list
   -- as an indicator of having no /errors/, so it would be a redundant
   -- wrapper.
   = EGChangeProcessingStatusResponseOk
   { errors :: Maybe [EGChangeProcessingStatusResponseError]
   }

   | EGChangeProcessingStatusResponseFailure
   { resultCode :: EGChangeProcessingStatusResponseFailureResultCode
   , description :: Maybe NonEmptyText
       -- ^ Optional textual comment for an error
   }

     deriving (Eq, Show, Generic)

instance FromJSON EGChangeProcessingStatusResponse where
  -- | Type annotation added here to provide type-variable @t@ inside
  --   (for type-safety reasons).
  parseJSON
    :: forall t final okConstructor failureConstructor errorsListType
     .
     ( t ~ EGChangeProcessingStatusResponse
     , final ~ Rep t
     , okConstructor ~ "EGChangeProcessingStatusResponseOk"
     , failureConstructor ~ "EGChangeProcessingStatusResponseFailure"
     , 'Just errorsListType ~ ConstructorFieldType final okConstructor "errors"
     )
    => Value
    -> Parser t

  parseJSON src = parse where
    resultCodeKey = fieldName' (Proxy :: Proxy '(final, "resultCode"))

    okConstructorProxy      = Proxy :: Proxy '(final, okConstructor)
    failureConstructorProxy = Proxy :: Proxy '(final, failureConstructor)

    parse = do
      obj <-
        -- Handling of optional @NonEmptyText@ fields
        preParseOptionalNonEmptyTextFieldsRep'
          (proxyPair2Triplet failureConstructorProxy (Proxy :: Proxy 1)) src

      let f = genericParseJSON defaultOptions . Object

      case HM.lookup resultCodeKey obj of
           Just (String x) | x == toStringy OK ->
             let
               proxy = Proxy :: Proxy '(final, okConstructor, errorsListType, 1)
               resolve = f . addConstructorTag' okConstructorProxy
             in
               preParseOptionalListFieldsRep' proxy (Object obj) >>= resolve

           -- It will fail in case @resultCode@ has incorrect type or not set
           -- at all since it's a required field for failure constructor.
           _ -> f $ addConstructorTag' failureConstructorProxy obj

instance ToJSON EGChangeProcessingStatusResponse where
  toJSON :: forall t. t ~ EGChangeProcessingStatusResponse => t -> Value

  toJSON EGChangeProcessingStatusResponseOk { errors } = object result where
    tConstructorProxy =
      Proxy :: Proxy '(t, "EGChangeProcessingStatusResponseOk")

    result =
      [ fieldName (Proxy :: Proxy '(t, "resultCode")) .= OK

      , constructorFieldName
          (proxyPair2Triplet tConstructorProxy
                             (Proxy :: Proxy "errors")) .= errors
      ]

  toJSON EGChangeProcessingStatusResponseFailure { resultCode, description }
    = object result where

    tConstructorProxy =
      Proxy :: Proxy '(t, "EGChangeProcessingStatusResponseFailure")

    result =
      [ constructorFieldName
          (proxyPair2Triplet tConstructorProxy
                             (Proxy :: Proxy "resultCode")) .= resultCode

      , constructorFieldName
          (proxyPair2Triplet tConstructorProxy
                             (Proxy :: Proxy "description")) .= description
      ]

instance ToSchema EGChangeProcessingStatusResponse where
  -- | Cutting off failure constructor and appending @\"resultCode"@ field
  --   (which is always @\"OK"@) to successful constructor.
  declareNamedSchema
    :: forall proxy t typeRep
                      resultCodeFieldName
                      successfulResultCodeField
                      final
     .
     ( t ~ EGChangeProcessingStatusResponse
     , typeRep ~ Rep t

     , -- Prooving we have this field in another constructor (see failure).
       'Just resultCodeFieldName ~ FieldName typeRep "resultCode"

     , -- New field definition to prepend.
       successfulResultCodeField ~
         S1 ('MetaSel ('Just resultCodeFieldName)
                      'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
            (Rec0 EGChangeProcessingStatusResponseSuccessfulResultCode)

     , -- Prepending new field to successful constructor.
       'Just final ~
         MapConstructorByName' "EGChangeProcessingStatusResponseOk"
                               ((:*:) successfulResultCodeField)
                               typeRep
     )
    => proxy t
    -> Declare (Definitions Schema) NamedSchema

  declareNamedSchema _ =
    gdeclareNamedSchema defaultSchemaOptions (Proxy :: Proxy final) mempty


-- | Just a plug for a proper Swagger instance of "EGBindVehiclesResponse".
data EGChangeProcessingStatusResponseSuccessfulResultCode = OK
     deriving (Eq, Show, Enum, Bounded, Generic)

instance StringyEnum EGChangeProcessingStatusResponseSuccessfulResultCode where
  toStringy OK = "OK"

instance ToJSON EGChangeProcessingStatusResponseSuccessfulResultCode where
  toJSON = String . toStringy

instance FromJSON EGChangeProcessingStatusResponseSuccessfulResultCode where
  parseJSON = parseStringyEnumJSON

instance ToSchema EGChangeProcessingStatusResponseSuccessfulResultCode where
  declareNamedSchema = stringyEnumNamedSchema


data EGChangeProcessingStatusResponseFailureResultCode
   = IncorrectFormat
   | InternalError
     deriving (Eq, Show, Enum, Bounded, Generic)

instance StringyEnum EGChangeProcessingStatusResponseFailureResultCode where
  toStringy IncorrectFormat = "INCORRECT_FORMAT"
  toStringy InternalError   = "INTERNAL_ERROR"

instance ToJSON EGChangeProcessingStatusResponseFailureResultCode where
  toJSON = String . toStringy

instance FromJSON EGChangeProcessingStatusResponseFailureResultCode where
  parseJSON = parseStringyEnumJSON

instance ToSchema EGChangeProcessingStatusResponseFailureResultCode where
  declareNamedSchema = stringyEnumNamedSchema


data EGChangeProcessingStatusResponseError
   = EGChangeProcessingStatusResponseError
   { requestId :: EGRequestId
       -- ^ Unique identity of a request for service an error happened with.
       --
       -- Currently only possible error is that such @EGRequestId@ not found.

   , errorCode :: EGChangeProcessingStatusResponseErrorCode

   } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)


data EGChangeProcessingStatusResponseErrorCode = RequestNotFound
     deriving (Eq, Show, Enum, Bounded, Generic)

instance StringyEnum EGChangeProcessingStatusResponseErrorCode where
  toStringy RequestNotFound = "REQUEST_NOT_FOUND"

instance ToJSON EGChangeProcessingStatusResponseErrorCode where
  toJSON = String . toStringy

instance FromJSON EGChangeProcessingStatusResponseErrorCode where
  parseJSON = parseStringyEnumJSON

instance ToSchema EGChangeProcessingStatusResponseErrorCode where
  declareNamedSchema = stringyEnumNamedSchema
