{-# LANGUAGE DeriveGeneric, DeriveAnyClass, ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, InstanceSigs #-}
{-# LANGUAGE OverloadedStrings, LambdaCase, DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns, QuasiQuotes #-}

-- Fixes issue when record-fields aren't exported. Probably related to:
--   https://stackoverflow.com/questions/46357747/haddock-data-record-fields-names-not-being-generated
{-# OPTIONS_HADDOCK ignore-exports #-}

module Carma.EraGlonass.Types.EGBindVehiclesRequest
     ( EGBindVehiclesRequest                   (..)
     , EGBindVehiclesMode                      (..)

     , EGBindVehiclesResponse                  (..)
     , EGBindVehiclesResponseFailureResultCode (..)
     , EGBindVehiclesResponseError             (..)

     , proof
     ) where

import           GHC.Generics

import           Data.Proxy
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.HashMap.Lazy as HM
import           Data.List.NonEmpty (NonEmpty)
import           Text.InterpolatedString.QM (qns)
import           Data.Swagger hiding (description)
import           Data.Swagger.Internal.Schema
import           Data.Swagger.Declare (Declare)

import           Carma.Utils.Operators
import           Carma.Utils.StringyEnum
import           Carma.Utils.StringyEnum.Aeson
import           Carma.Utils.StringyEnum.SwaggerSchema
import           Carma.Utils.TypeSafe.Proxy
import           Carma.Utils.TypeSafe.Generic.Record
import           Carma.Utils.TypeSafe.Generic.Aeson
import           Carma.Utils.TypeSafe.Generic.Record.Operations.ReplaceFieldType
import           Carma.Utils.TypeSafe.Generic.DataType.Operations.MapConstructor
import           Carma.EraGlonass.Types.Helpers.Proof
import           Carma.EraGlonass.Types.Helpers.Aeson
import           Carma.EraGlonass.Types.Helpers.NonEmptyText
import           Carma.EraGlonass.Types.EGContractId (EGContractId)
import           Carma.EraGlonass.Types.EGVin (EGVin)


proof :: ()
proof
  = proofThatTheTypeIsComplete (Proxy :: Proxy EGBindVehiclesRequest)
  ! proofThatTheTypeIsComplete (Proxy :: Proxy EGBindVehiclesMode)

  ! proofThatTheTypeIsComplete (Proxy :: Proxy EGBindVehiclesResponse)

  ! proofThatTheTypeIsComplete
      (Proxy :: Proxy EGBindVehiclesResponseFailureResultCode)

  ! proofThatTheTypeIsComplete (Proxy :: Proxy EGBindVehiclesResponseError)


data EGBindVehiclesRequest
   = EGBindVehiclesRequest
   { contractId :: EGContractId
       -- ^ Identity of a contract of a service provider
       --   (CaRMa's unique id given by EG).

   , mode :: EGBindVehiclesMode
       -- ^ Action that indicates whether we /bind/ or /unbind/ VINs.

   , vins :: NonEmpty EGVin
       -- ^ A list of VINs to bind/unbind.
       --
       -- One element is minimum, that's why it's "NonEmpty".
       --
       -- See "EGVinsNonEmptyList" for a Swagger schema of this field.

   } deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ToSchema EGBindVehiclesRequest where
  declareNamedSchema
    :: forall proxy t final.
     ( t ~ EGBindVehiclesRequest

     , '(1, final) ~
         ReplaceFieldTypeFromToByFieldName
           (Rep t) "vins" (NonEmpty EGVin) EGVinsNonEmptyList
     )
    => proxy t
    -> Declare (Definitions Schema) NamedSchema

  declareNamedSchema _ =
    gdeclareNamedSchema defaultSchemaOptions (Proxy :: Proxy final) mempty

-- | Just a helper type plug for producing correct
--   "EGBindVehiclesRequest"'s Swagger schema.
data EGVinsNonEmptyList

instance ToSchema EGVinsNonEmptyList where
  declareNamedSchema _ =
    declareNamedSchema (Proxy :: Proxy [EGVin]) <&> \case
      x@NamedSchema { _namedSchemaSchema = schema' } -> x
        { _namedSchemaSchema
            = schema'
            { _schemaMinProperties = Just 1
            , _schemaDescription   = Just
                [qns| One VIN is minimum.
                      All VINs are supposed to be unique. |]
            }
        }


-- | Helper type to specify an operation for a request.
data EGBindVehiclesMode = Bind | Unbind
     deriving (Eq, Show, Enum, Bounded, Generic)

instance StringyEnum EGBindVehiclesMode where
  toStringy Bind   = "BIND"
  toStringy Unbind = "UNBIND"

instance ToJSON   EGBindVehiclesMode where toJSON    = String . toStringy
instance FromJSON EGBindVehiclesMode where parseJSON = parseStringyEnumJSON

instance ToSchema EGBindVehiclesMode where
  declareNamedSchema = stringyEnumNamedSchema


data EGBindVehiclesResponse
   -- | When @resultCode@ equals to @\"OK"@.
   --
   -- For now @errors@ can be only filled with VINs which are not found in the
   -- EG's database. See @VinNotFound@ @errorCode@.
   --
   -- @errors@ could be empty list or I believe (but not sure) could be not
   -- set at all, so we have to add proper implementation for that in
   -- @FromJSON@ instance. We could add @Maybe@ but we already have empty list
   -- as an indicator of having no /errors/, so it would be a redundant
   -- wrapper.
   = EGBindVehiclesResponseOk
   { errors :: [EGBindVehiclesResponseError]
       -- ^ A list of VINs which are not found (there's only one possible error
       --   for a VIN at the moment of implementation - @\"VIN_NOT_FOUND"@).
       --
       -- Following common sense we can assume this field may be filled only in
       -- case the action is @\"UNBIND"@ because there's nothing to be \"not
       -- found" when we're @\"BIND"@ing new VINs to that list of vehicles
       -- handled by us (by CaRMa).
   }

   -- | When @resultCode@ is not equals to \"OK".
   | EGBindVehiclesResponseFailure
   { resultCode  :: EGBindVehiclesResponseFailureResultCode
   , description :: Maybe NonEmptyText
       -- ^ Optional textual comment for an error
   }

     deriving (Eq, Show, Generic)

instance FromJSON EGBindVehiclesResponse where
  -- | Type annotation added here to provide type-variable @t@ inside
  --   (for type-safety reasons).
  parseJSON
    :: forall t final okConstructor failureConstructor errorsListType
     .
     ( t ~ EGBindVehiclesResponse
     , final ~ Rep t
     , okConstructor ~ "EGBindVehiclesResponseOk"
     , failureConstructor ~ "EGBindVehiclesResponseFailure"
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

instance ToJSON EGBindVehiclesResponse where
  toJSON :: forall t. t ~ EGBindVehiclesResponse => t -> Value

  toJSON EGBindVehiclesResponseOk { errors } = object result where
    tConstructorProxy = Proxy :: Proxy '(t, "EGBindVehiclesResponseOk")

    result =
      [ fieldName (Proxy :: Proxy '(t, "resultCode")) .= OK

      , constructorFieldName
          (proxyPair2Triplet tConstructorProxy
                             (Proxy :: Proxy "errors")) .= errors
      ]

  toJSON EGBindVehiclesResponseFailure { resultCode, description }
    = object result where

    tConstructorProxy =
      Proxy :: Proxy '(t, "EGBindVehiclesResponseFailure")

    result =
      [ constructorFieldName
          (proxyPair2Triplet tConstructorProxy
                             (Proxy :: Proxy "resultCode")) .= resultCode

      , constructorFieldName
          (proxyPair2Triplet tConstructorProxy
                             (Proxy :: Proxy "description")) .= description
      ]

instance ToSchema EGBindVehiclesResponse where
  -- | Cutting off failure constructor and appending @\"resultCode"@ field
  --   (which is always @\"OK"@) to successful constructor.
  declareNamedSchema
    :: forall proxy t typeRep
                      resultCodeFieldName
                      successfulResultCodeField
                      final
     .
     ( t ~ EGBindVehiclesResponse
     , typeRep ~ Rep t

     , -- Prooving we have this field in another constructor (see failure).
       'Just resultCodeFieldName ~ FieldName typeRep "resultCode"

     , -- New field definition to prepend.
       successfulResultCodeField ~
         S1 ('MetaSel ('Just resultCodeFieldName)
                      'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
            (Rec0 EGBindVehiclesResponseSuccessfulResultCode)

     , -- Prepending new field to successful constructor.
       'Just final ~
         MapConstructorByName' "EGBindVehiclesResponseOk"
                               ((:*:) successfulResultCodeField)
                               typeRep
     )
    => proxy t
    -> Declare (Definitions Schema) NamedSchema

  declareNamedSchema _ =
    gdeclareNamedSchema defaultSchemaOptions (Proxy :: Proxy final) mempty


-- | Just a plug for a proper Swagger instance of "EGBindVehiclesResponse".
data EGBindVehiclesResponseSuccessfulResultCode = OK
     deriving (Eq, Show, Enum, Bounded, Generic)

instance StringyEnum EGBindVehiclesResponseSuccessfulResultCode where
  toStringy OK = "OK"

instance ToJSON EGBindVehiclesResponseSuccessfulResultCode where
  toJSON = String . toStringy

instance FromJSON EGBindVehiclesResponseSuccessfulResultCode where
  parseJSON = parseStringyEnumJSON

instance ToSchema EGBindVehiclesResponseSuccessfulResultCode where
  declareNamedSchema = stringyEnumNamedSchema


data EGBindVehiclesResponseFailureResultCode
   = IncorrectFormat
   | ContractNotFound
   | InternalError
     deriving (Eq, Show, Enum, Bounded, Generic)

instance StringyEnum EGBindVehiclesResponseFailureResultCode where
  toStringy IncorrectFormat  = "INCORRECT_FORMAT"
  toStringy ContractNotFound = "CONTRACT_NOT_FOUND"
  toStringy InternalError    = "INTERNAL_ERROR"

instance ToJSON EGBindVehiclesResponseFailureResultCode where
  toJSON = String . toStringy

instance FromJSON EGBindVehiclesResponseFailureResultCode where
  parseJSON = parseStringyEnumJSON

instance ToSchema EGBindVehiclesResponseFailureResultCode where
  declareNamedSchema = stringyEnumNamedSchema


data EGBindVehiclesResponseError
   = EGBindVehiclesResponseError
   { vin       :: EGVin
   , errorCode :: EGBindVehiclesResponseErrorCode
   } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)


data EGBindVehiclesResponseErrorCode = VinNotFound
     deriving (Eq, Show, Enum, Bounded, Generic)

instance StringyEnum EGBindVehiclesResponseErrorCode where
  toStringy VinNotFound = "VIN_NOT_FOUND"

instance ToJSON EGBindVehiclesResponseErrorCode where
  toJSON = String . toStringy

instance FromJSON EGBindVehiclesResponseErrorCode where
  parseJSON = parseStringyEnumJSON

instance ToSchema EGBindVehiclesResponseErrorCode where
  declareNamedSchema = stringyEnumNamedSchema
