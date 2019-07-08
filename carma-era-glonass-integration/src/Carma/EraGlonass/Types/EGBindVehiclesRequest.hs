{-# LANGUAGE DeriveGeneric, DeriveAnyClass, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, DataKinds, TypeOperators, TypeFamilies, InstanceSigs #-}
{-# LANGUAGE OverloadedStrings, LambdaCase, DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns, QuasiQuotes, NoMonomorphismRestriction #-}

-- Fixes issue when record-fields aren't exported. Probably related to:
--   https://stackoverflow.com/questions/46357747/haddock-data-record-fields-names-not-being-generated
{-# OPTIONS_HADDOCK ignore-exports #-}

module Carma.EraGlonass.Types.EGBindVehiclesRequest
     ( EGBindVehiclesRequest                   (..)
     , EGBindVehiclesResponse                  (..)

     , EGBindVehiclesMode                      (..)
     , EGBindVehiclesResponseFailureResultCode (..)
     , EGBindVehiclesResponseError             (..)
     , EGBindVehiclesResponseErrorCode         (..)

     , proof
     ) where

import           GHC.Generics
import           GHC.TypeLits
import           Generics.Deriving.Eq
import           Generics.Deriving.Show

import           Data.Proxy
import           Data.Type.Equality
import           Data.Aeson
import           Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.HashMap.Lazy as HM
import           Data.List.NonEmpty (NonEmpty)
import           Data.String (IsString (fromString))
import           Text.InterpolatedString.QM
import           Data.Swagger hiding (description)
import           Data.Swagger.Internal.Schema
import           Data.Swagger.Declare (Declare)

import           Carma.Utils.Operators
import           Carma.Utils.StringyEnum
import           Carma.Utils.StringyEnum.Aeson
import           Carma.Utils.StringyEnum.SwaggerSchema
import           Carma.Utils.GenericInstance
import           Carma.Utils.TypeSafe.Proxy
import           Carma.Utils.TypeSafe.TypeFamilies
import           Carma.Utils.TypeSafe.Serialize
import           Carma.Utils.TypeSafe.KindToType
import           Carma.Utils.TypeSafe.Generic.DataType
import           Carma.Utils.TypeSafe.Generic.Record
import           Carma.Utils.TypeSafe.Generic.Aeson
import           Carma.Utils.TypeSafe.Generic.DataType.Operations.MapConstructor
import           Carma.Utils.TypeSafe.Generic.Record.Operations.ReplaceFieldType
import           Carma.Utils.TypeSafe.Generic.Record.Operations.GetFieldNamesByType
import           Carma.EraGlonass.Types.Helpers.Proof
import           Carma.EraGlonass.Types.Helpers.Aeson
import           Carma.EraGlonass.Types.Helpers.NonEmptyText
import           Carma.EraGlonass.Types.EGContractId (EGContractId)
import           Carma.EraGlonass.Types.EGVin (EGVin)
import           Carma.EraGlonass.Types.EGIntegrationPoint
                   ( EGIntegrationPoint (BindVehicles)
                   )


proof :: ()
proof
  = proofThatTheTypeIsComplete (Proxy :: Proxy (EGBindVehiclesRequest 'Bind))
  ! proofThatTheTypeIsComplete (Proxy :: Proxy (EGBindVehiclesRequest 'Unbind))

  ! proofThatTheTypeIsComplete (Proxy :: Proxy (EGBindVehiclesResponse 'Bind))
  ! proofThatTheTypeIsComplete (Proxy :: Proxy (EGBindVehiclesResponse 'Unbind))

  ! proofThatTheTypeIsComplete (Proxy :: Proxy EGBindVehiclesMode)

  ! proofThatTheTypeIsComplete
      (Proxy :: Proxy EGBindVehiclesResponseFailureResultCode)

  ! proofThatTheTypeIsComplete (Proxy :: Proxy EGBindVehiclesResponseError)
  ! proofThatTheTypeIsComplete (Proxy :: Proxy EGBindVehiclesResponseErrorCode)


data EGBindVehiclesRequest (mode :: EGBindVehiclesMode)
   = EGBindVehiclesRequest
   { contractId :: EGContractId 'BindVehicles
       -- ^ Identity of a contract of a service provider
       --   (CaRMa's unique id given by EG).

   , vins :: NonEmpty EGVin
       -- ^ A list of VINs to bind/unbind.
       --
       -- One element is minimum, that's why it's "NonEmpty".
       --
       -- See "EGVinsNonEmptyList" for a Swagger schema of this field.

   } deriving (Eq, Show, Generic)

-- | Adds "mode" field to "EGBindVehiclesRequest".
type family EGBindVehiclesRequestAddModeField
            (typeRep :: * -> *)
            (mode    :: EGBindVehiclesMode)
                     :: Maybe (* -> *)
                     where
  EGBindVehiclesRequestAddModeField typeRep mode =
    MapConstructorByName typeRep "EGBindVehiclesRequest" ((:*:) (
      S1 ('MetaSel ('Just "mode")
                   'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
         (Rec0 (EGBindVehiclesModeOnly mode))
    ))

instance ( EGBindVehiclesModeToTermLevel mode
         , t ~ EGBindVehiclesRequest mode

         , 'Just typeRepWithMode ~
             EGBindVehiclesRequestAddModeField (Rep t) mode

         ) => ToJSON (EGBindVehiclesRequest mode) where

  toJSON :: t -> Value
  toJSON = go where
    go = addModeField . genericToJSON defaultOptions
    mode = egBindVehiclesModeToTermLevel (Proxy :: Proxy mode)

    modeFieldName =
      constructorFieldName'
        (Proxy :: Proxy '(typeRepWithMode, "EGBindVehiclesRequest", "mode"))

    addModeField (Object x) = Object $ HM.insert modeFieldName (toJSON mode) x
    addModeField x = error
      [qms| ToJSON {typeName (Proxy :: Proxy t) :: String}:
            Result is unexpectedly not an Object: {x} |]

instance ( EGBindVehiclesModeToTermLevel mode
         , t ~ EGBindVehiclesRequest mode

         , 'Just typeRepWithMode ~
             EGBindVehiclesRequestAddModeField (Rep t) mode

         ) => FromJSON (EGBindVehiclesRequest mode) where

  parseJSON :: Value -> Parser t
  parseJSON = go where
    mode = egBindVehiclesModeToTermLevel (Proxy :: Proxy mode)

    modeFieldName =
      constructorFieldName'
        (Proxy :: Proxy '(typeRepWithMode, "EGBindVehiclesRequest", "mode"))

    go src = do
      obj <- extrudeObject (Proxy :: Proxy t) src

      case HM.lookup modeFieldName obj of
           Just x@(String _)
             | x == toJSON mode -> pure ()
             | otherwise -> fail [qms|
                 {typeName (Proxy :: Proxy t) :: String}: incorrect mode,
                 expected {toJSON mode}, received: {x}
               |]
           Just x ->
             typeMismatch (typeName $ kindToType (Proxy :: Proxy mode)) x
           Nothing ->
             typeMismatch (typeName $ kindToType (Proxy :: Proxy mode)) src

      genericParseJSON defaultOptions $ Object obj

instance ToSchema (EGBindVehiclesModeOnly mode)
      => ToSchema (EGBindVehiclesRequest mode) where

  declareNamedSchema
    :: forall proxy t withNonEmptyVinsList final.
     ( t ~ EGBindVehiclesRequest mode

     , '(1, withNonEmptyVinsList) ~
         ReplaceFieldTypeFromToByFieldName
           (Rep t) "vins" (NonEmpty EGVin) EGVinsNonEmptyList

     , 'Just final ~ EGBindVehiclesRequestAddModeField withNonEmptyVinsList mode
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


-- | GADT here to share failure constructor for both @Bind@ and @Unbind@ modes
--   but separating OK constructors which are different for each of mode.
--
-- OK constructors for each mode are constrained to be able to be created only
-- for specific mode it belongs to.
data EGBindVehiclesResponse (mode :: EGBindVehiclesMode) where
   -- | When @resultCode@ equals to @\"OK"@.
   --
   -- We have this separated OK constructor because @errors@ are only for
   -- unbinding, when a VIN not found on EG side in database, so it's already
   -- unbounded or have never been bounded before.
   EGBindVehiclesResponseBindOk :: EGBindVehiclesResponse 'Bind

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
   EGBindVehiclesResponseUnbindOk ::
     { errors :: [EGBindVehiclesResponseError]
         -- ^ A list of VINs which are not found (there's only one possible error
         --   for a VIN at the moment of implementation - @\"VIN_NOT_FOUND"@).
         --
         -- Following common sense we can assume this field may be filled only in
         -- case the action is @\"UNBIND"@ because there's nothing to be \"not
         -- found" when we're @\"BIND"@ing new VINs to that list of vehicles
         -- handled by us (by CaRMa).
     } -> EGBindVehiclesResponse 'Unbind

   -- | When @resultCode@ is not equals to \"OK".
   EGBindVehiclesResponseFailure ::
     { resultCode  :: EGBindVehiclesResponseFailureResultCode
     , description :: Maybe NonEmptyText
         -- ^ Optional textual comment for an error
     } -> EGBindVehiclesResponse mode

-- | WARNING! Since we can't derive instances for GADTs automatically (when we
--            have type equality constraints, for bind/unbind *ok*-cases),
--            this one is written manually, be careful and attentive, keep this
--            instance up-to-date with original datatype, errors in this
--            instance will lead to errors in other instances such as
--            @FromJSON@, @ToJSON@ and @ToSchema@. The most prone to human
--            errors places are field names, constructors and types of fields
--            are pretty type-level proofed by @from@ and @to@ implementations.
instance Generic (EGBindVehiclesResponse 'Bind) where
  type Rep (EGBindVehiclesResponse 'Bind) =
    ProtoDatatype (Rep EGBindVehiclesMode) "EGBindVehiclesResponse"
      '[ EmptyConstructor  "EGBindVehiclesResponseBindOk"
       , RecordConstructor "EGBindVehiclesResponseFailure"
           '[ '("resultCode",  EGBindVehiclesResponseFailureResultCode)
            , '("description", Maybe NonEmptyText)
            ]
       ]

  from EGBindVehiclesResponseBindOk =
    M1 $ L1 $ M1 U1
  from (EGBindVehiclesResponseFailure a b) =
    M1 $ R1 $ M1 $ M1 (K1 a) :*: M1 (K1 b)

  to (M1 (L1 (M1 U1))) =
    EGBindVehiclesResponseBindOk
  to (M1 (R1 (M1 (M1 (K1 a) :*: M1 (K1 b))))) =
    EGBindVehiclesResponseFailure a b

-- | WARNING! Since we can't derive instances for GADTs automatically (when we
--            have type equality constraints, for bind/unbind *ok*-cases),
--            this one is written manually, be careful and attentive, keep this
--            instance up-to-date with original datatype, errors in this
--            instance will lead to errors in other instances such as
--            @FromJSON@, @ToJSON@ and @ToSchema@. The most prone to human
--            errors places are field names, constructors and types of fields
--            are pretty type-level proofed by @from@ and @to@ implementations.
instance Generic (EGBindVehiclesResponse 'Unbind) where
  type Rep (EGBindVehiclesResponse 'Unbind) =
    ProtoDatatype (Rep EGBindVehiclesMode) "EGBindVehiclesResponse"
      '[ RecordConstructor "EGBindVehiclesResponseUnbindOk"
           '[ '("errors", [EGBindVehiclesResponseError])
            ]
       , RecordConstructor "EGBindVehiclesResponseFailure"
           '[ '("resultCode",  EGBindVehiclesResponseFailureResultCode)
            , '("description", Maybe NonEmptyText)
            ]
       ]

  from (EGBindVehiclesResponseUnbindOk a) =
    M1 $ L1 $ M1 $ M1 $ K1 a
  from (EGBindVehiclesResponseFailure a b) =
    M1 $ R1 $ M1 $ M1 (K1 a) :*: M1 (K1 b)

  to (M1 (L1 (M1 (M1 (K1 a))))) =
    EGBindVehiclesResponseUnbindOk a
  to (M1 (R1 (M1 (M1 (K1 a) :*: M1 (K1 b))))) =
    EGBindVehiclesResponseFailure a b

instance GEq (EGBindVehiclesResponse 'Bind)
instance Eq  (EGBindVehiclesResponse 'Bind) where (==) = geq

instance GEq (EGBindVehiclesResponse 'Unbind)
instance Eq  (EGBindVehiclesResponse 'Unbind) where (==) = geq

instance GShow (EGBindVehiclesResponse 'Bind)
instance Show  (EGBindVehiclesResponse 'Bind) where
  showsPrec = gshowsPrecdefault

instance GShow (EGBindVehiclesResponse 'Unbind)
instance Show  (EGBindVehiclesResponse 'Unbind) where
  showsPrec = gshowsPrecdefault

-- | Converts specific mode to associated OK constructor of result type.
type family EGBindVehiclesResponseOkConstructorNameByMode
            (mode :: EGBindVehiclesMode) :: Symbol
            where
  EGBindVehiclesResponseOkConstructorNameByMode 'Bind =
    "EGBindVehiclesResponseBindOk"
  EGBindVehiclesResponseOkConstructorNameByMode 'Unbind =
    "EGBindVehiclesResponseUnbindOk"

-- | Polymorphic parser for OK constructors for each mode of result type.
class EGBindVehiclesResponseParseJsonOk (mode :: EGBindVehiclesMode) where
  egBindVehiclesResponseParseJsonOk :: Proxy mode -> Object -> Parser Object

instance EGBindVehiclesResponseParseJsonOk 'Bind where
  egBindVehiclesResponseParseJsonOk Proxy = pure

instance EGBindVehiclesResponseParseJsonOk 'Unbind where
  egBindVehiclesResponseParseJsonOk
    :: forall t mode final constructor errorsListType
     .
     ( t ~ EGBindVehiclesResponse mode
     , mode ~ 'Unbind
     , final ~ Rep t
     , constructor ~ "EGBindVehiclesResponseUnbindOk"
     , ConstructorName final constructor ~ 'Just constructor
     , 'Just errorsListType ~ ConstructorFieldType final constructor "errors"
     )
    => Proxy mode
    -> Object
    -> Parser Object

  egBindVehiclesResponseParseJsonOk Proxy = go where
    proxy = Proxy :: Proxy '(final, constructor, errorsListType, 1)
    go    = preParseOptionalListFieldsRep' proxy . Object

instance ( t ~ EGBindVehiclesResponse mode
         , Generic t
         , final ~ Rep t
         , okConstructor ~ EGBindVehiclesResponseOkConstructorNameByMode mode
         , failureConstructor ~ "EGBindVehiclesResponseFailure"
         , EGBindVehiclesResponseParseJsonOk mode
         , TypeName final ~ typeName
         , FieldName final "resultCode" ~ 'Just resultCodeFieldName
         , ConstructorName final failureConstructor ~ 'Just failureConstructor
         , ConstructorName final okConstructor ~ 'Just okConstructor
         , KnownSymbol typeName
         , KnownSymbol okConstructor
         , KnownSymbol resultCodeFieldName
         , GFromJSON Zero final

         , nonEmptyTextFieldNames ~
             GetFieldNamesOfConstructorByType
               final failureConstructor (Maybe NonEmptyText)

         , 1 ~ Length nonEmptyTextFieldNames
         , SerializableListOfKnownSymbols nonEmptyTextFieldNames
         ) => FromJSON (EGBindVehiclesResponse mode) where

  parseJSON :: Value -> Parser t
  parseJSON src = parse where
    okConstructorProxy      = Proxy :: Proxy '(final, okConstructor)
    failureConstructorProxy = Proxy :: Proxy '(final, failureConstructor)

    resultCodeFieldName =
      fromString $ symbolVal (Proxy :: Proxy resultCodeFieldName)

    parse = do
      obj <-
        -- Handling of optional @NonEmptyText@ fields
        preParseOptionalNonEmptyTextFieldsRep'
          (proxyPair2Triplet failureConstructorProxy (Proxy :: Proxy 1)) src

      let f = genericParseJSON defaultOptions . Object

      case HM.lookup resultCodeFieldName obj of
           Just (String x) | x == toStringy OK ->
             egBindVehiclesResponseParseJsonOk (Proxy :: Proxy mode) obj
               >>= f . addConstructorTag' okConstructorProxy

           -- It will fail in case @resultCode@ has incorrect type or not set
           -- at all since it's a required field for failure constructor.
           _ -> f $ addConstructorTag' failureConstructorProxy obj

instance ( t ~ EGBindVehiclesResponse mode
         , final ~ Rep t
         , failureConstructor ~ "EGBindVehiclesResponseFailure"
         , KnownSymbol failureConstructor
         , ConstructorName final failureConstructor ~ 'Just failureConstructor
         , resultCodeFieldName ~ "resultCode"
         , descriptionFieldName ~ "description"
         , KnownSymbol resultCodeFieldName
         , KnownSymbol descriptionFieldName

         , ConstructorFieldName final failureConstructor resultCodeFieldName
             ~ 'Just resultCodeFieldName
         , ConstructorFieldName final failureConstructor descriptionFieldName
             ~ 'Just descriptionFieldName

         ) => ToJSON (EGBindVehiclesResponse mode) where

  toJSON :: t -> Value
  toJSON = object . fields where
    f = fromString . symbolVal
    fields = \case
      EGBindVehiclesResponseBindOk ->
        [ f (Proxy :: Proxy resultCodeFieldName) .= OK
        ]
      EGBindVehiclesResponseUnbindOk { errors } ->
        let
          errorsFieldNameProxy
            ::
             ( fieldName ~ "errors"
             , KnownSymbol fieldName
             , 'Just fieldName ~
                 ConstructorFieldName final
                   (EGBindVehiclesResponseOkConstructorNameByMode mode)
                   fieldName
             )
            => Proxy fieldName

          errorsFieldNameProxy = Proxy
        in
          [ f (Proxy :: Proxy resultCodeFieldName) .= OK
          , f errorsFieldNameProxy                 .= errors
          ]
      EGBindVehiclesResponseFailure { resultCode, description } ->
        [ f (Proxy :: Proxy resultCodeFieldName)  .= resultCode
        , f (Proxy :: Proxy descriptionFieldName) .= description
        ]

-- | Helper to add @resultCode@ field to OK constructor of response type.
--
-- Works both for @Bind@ and @Unbind@ modes.
type family EGBindVehiclesResponseResultCodeOkFieldByMode
            (mode    :: EGBindVehiclesMode)
            (field   :: * -> *)
            (typeRep :: * -> *)
                     :: * -> *
                     where

  EGBindVehiclesResponseResultCodeOkFieldByMode 'Bind field
    (D1 m (C1 ('MetaCons n f 'False) U1 :+: t)) =
      Guard (n == EGBindVehiclesResponseOkConstructorNameByMode 'Bind)
            (D1 m (C1 ('MetaCons n f 'True) field :+: t))

  EGBindVehiclesResponseResultCodeOkFieldByMode 'Unbind field typeRep =
    FromJust (
      MapConstructorByName typeRep
        (EGBindVehiclesResponseOkConstructorNameByMode 'Unbind) ((:*:) field)
    )

instance ( t ~ EGBindVehiclesResponse mode
         , typeRep ~ Rep t
         , GToSchema final

         , -- Prooving we have this field in another constructor (see failure).
           'Just resultCodeFieldName ~ FieldName typeRep "resultCode"

         , -- New field definition to prepend.
           successfulResultCodeField ~
             S1 ('MetaSel ('Just resultCodeFieldName)
                          'NoSourceUnpackedness
                          'NoSourceStrictness
                          'DecidedLazy)
                (Rec0 EGBindVehiclesResponseSuccessfulResultCode)

         , -- Prepending new field to successful constructor.
           final ~
             EGBindVehiclesResponseResultCodeOkFieldByMode
               mode successfulResultCodeField typeRep

         ) => ToSchema (EGBindVehiclesResponse mode) where

  declareNamedSchema :: proxy t -> Declare (Definitions Schema) NamedSchema
  declareNamedSchema _ =
    gdeclareNamedSchema defaultSchemaOptions (Proxy :: Proxy final) mempty


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

-- | Shifts "EGBindVehiclesMode" from type-level to term-level.
class EGBindVehiclesModeToTermLevel (mode :: EGBindVehiclesMode) where
  egBindVehiclesModeToTermLevel :: Proxy mode -> EGBindVehiclesMode
instance EGBindVehiclesModeToTermLevel 'Bind where
  egBindVehiclesModeToTermLevel Proxy = Bind
instance EGBindVehiclesModeToTermLevel 'Unbind where
  egBindVehiclesModeToTermLevel Proxy = Unbind

-- | Either @Bind@ or "Unbind".
data EGBindVehiclesModeOnly (mode :: EGBindVehiclesMode)

instance ( t ~ EGBindVehiclesModeOnly mode
         , EGBindVehiclesModeToTermLevel mode
         ) => ToSchema (EGBindVehiclesModeOnly mode) where

  declareNamedSchema :: proxy t -> Declare (Definitions Schema) NamedSchema
  declareNamedSchema _
    = stringyEnumMappedNamedSchema (kindToType (Proxy :: Proxy mode))
    $ \(NamedSchema name' schema') -> pure
    $ NamedSchema name' schema'
    { _schemaParamSchema = (_schemaParamSchema schema')
        { _paramSchemaEnum
            = Just $ pure $ String $ toStringy
            $ egBindVehiclesModeToTermLevel (Proxy :: Proxy mode)
        }
    }


-- | Just a plug for a proper Swagger instance of "EGBindVehiclesResponse".
data EGBindVehiclesResponseSuccessfulResultCode = OK
     deriving (GEq, Eq, GShow, Show, Enum, Bounded, Generic)

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
     deriving (GEq, Eq, GShow, Show, Enum, Bounded, Generic)

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
   } deriving (GEq, Eq, GShow, Show, Generic, ToJSON, FromJSON, ToSchema)


data EGBindVehiclesResponseErrorCode = VinNotFound
     deriving (GEq, Eq, GShow, Show, Enum, Bounded, Generic)

instance StringyEnum EGBindVehiclesResponseErrorCode where
  toStringy VinNotFound = "VIN_NOT_FOUND"

instance ToJSON EGBindVehiclesResponseErrorCode where
  toJSON = String . toStringy

instance FromJSON EGBindVehiclesResponseErrorCode where
  parseJSON = parseStringyEnumJSON

instance ToSchema EGBindVehiclesResponseErrorCode where
  declareNamedSchema = stringyEnumNamedSchema
