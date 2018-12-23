{-# LANGUAGE DuplicateRecordFields, OverloadedLists #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}

-- Fixes issue when record-fields aren't exported. Probably related to:
--   https://stackoverflow.com/questions/46357747/haddock-data-record-fields-names-not-being-generated
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Data-types for adding VIN to list of VINs handled by CaRMa (__CRM.EG.02__),
-- request and response.
module Carma.EraGlonass.Types.EGAddVinRequest
     ( EGAddVinRequest (..)
     , EGAddVinRequestRequests (..)
     , EGAddVinResponse (..)
     , EGAddVinResponseResponses (..)
     ) where

import           GHC.Generics
import           GHC.TypeLits
import           GHC.Exts (IsList (..))

import           Data.Proxy
import           Data.Monoid
import           Data.String (fromString)
import           Data.Text (Text)
import           Text.InterpolatedString.QM
import qualified Data.HashMap.Lazy as HM
import qualified Data.Set as Set
import           Data.Aeson
import           Data.Aeson.TH (Options (omitNothingFields))
import           Data.Aeson.Types (Parser, typeMismatch, parseEither)
import           Data.Swagger
import           Data.Swagger.Internal.Schema
import           Data.Swagger.Declare (Declare)

import           Carma.Utils.StringyEnum
import           Carma.Utils.TypeSafe.Proxy
import           Carma.Utils.TypeSafe.Generic.DataType
import           Carma.Utils.TypeSafe.Generic.Record
import           Carma.Utils.TypeSafe.Generic.Aeson
import           Carma.Utils.TypeSafe.Generic.Swagger
import           Carma.EraGlonass.Types.Helpers
import           Carma.EraGlonass.Types.RequestId (RequestId)
import           Carma.EraGlonass.Types.EGVin (EGVin)
import           Carma.EraGlonass.Types.EGVinOperationAcceptCode
                   ( EGVinOperationAcceptCode (OK)
                   , EGVinOperationFailureAcceptCode
                   )


-- *** Request


data EGAddVinRequest
   = EGAddVinRequest
   { requestId :: RequestId
   , requests :: [EGAddVinRequestRequests]
   } deriving (Eq, Show, Generic, ToSchema)

instance ToJSON EGAddVinRequest where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }


data EGAddVinRequestRequests
   = EGAddVinRequestRequests
   { vin :: EGVin
   } deriving (Eq, Show, Generic)

instance ToJSON EGAddVinRequestRequests where
  toJSON (EGAddVinRequestRequests (vin' :: EGVin)) =
    object [fromString (symbolVal (Proxy :: Proxy VinFieldName)) .= vin']

instance ToSchema EGAddVinRequestRequests where
  declareNamedSchema _ = gdeclareNamedSchema defaultSchemaOptions proxy mempty
    where proxy :: Proxy (UppercaseVinField (Rep EGAddVinRequestRequests))
          proxy = Proxy

type family UppercaseVinField (k1 :: * -> *) where
  UppercaseVinField (D1 a (C1 x field)) =
    D1 a (C1 x (ReplaceFieldKey "vin" VinFieldName field))

type VinFieldName = "VIN"


-- *** Response


data EGAddVinResponse
   = EGAddVinResponseIncorrect
   { errorMessage :: String
   , incorrectResponseBody :: Value
   }

   | EGAddVinResponse
   { requestId :: RequestId
   , responses :: [EGAddVinResponseResponses]
   }
     deriving (Eq, Show, Generic)

instance FromJSON EGAddVinResponse where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  parseJSON :: forall t. t ~ EGAddVinResponse => Value -> Parser t
  parseJSON src = pure $
    -- Parsing here to extract parsing error message
    case parseEither (const successfulCase) src of
         Left msg -> EGAddVinResponseIncorrect msg src
         Right x  -> x

    where
      mismatch = typeMismatch (typeName (Proxy :: Proxy t)) src
      okConstructorProxy = Proxy :: Proxy '(t, "EGAddVinResponse")

      successfulCase = do
        obj <- -- Extracting hash-map from JSON @Object@
          case src of
               Object x -> pure x
               _        -> mismatch

        genericParseJSON defaultOptions $
          -- Associating it with successful case constructor
          Object $ addConstructorTag okConstructorProxy obj

type FailureConsMeta
   = 'MetaCons "EGAddVinResponseIncorrect" 'PrefixI 'True

-- | Slicing 'EGAddVinResponseIncorrect' constructor from Swagger spec.
type family CutOffFailureCons (k1 :: * -> *) where
  CutOffFailureCons (D1 a (C1 FailureConsMeta _ :+: y)) = D1 a y

instance ToSchema EGAddVinResponse where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  declareNamedSchema
    :: forall proxy t t2
    .  ( t ~ EGAddVinResponse
       , t2 ~ CutOffFailureCons (Rep t)
       )
    => proxy t
    -> Declare (Definitions Schema) NamedSchema

  declareNamedSchema _ =
    gdeclareNamedSchema defaultSchemaOptions (Proxy :: Proxy t2) mempty


-- | List item type of "responses" field of "EGAddVinResponse"
data EGAddVinResponseResponses
   = EGAddVinResponseResponsesOk
   { statusDescription :: Maybe Text
   , vin :: EGVin
   } -- ^ When @acceptCode@ is @\"OK"@

   | EGAddVinResponseResponsesFailure
   { acceptCode :: EGVinOperationFailureAcceptCode
   , statusDescription :: Maybe Text
   } -- ^ When @acceptCode@ is not @\"OK"@

     deriving (Eq, Show, Generic)

instance FromJSON EGAddVinResponseResponses where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  parseJSON
    :: forall t typeRep
    .  ( t ~ EGAddVinResponseResponses
       , typeRep ~ Rep t
       )
    => Value
    -> Parser t

  parseJSON src@(Object obj) = go where
    go = if Set.member acceptCodeKey keys then branching else mismatch

    branching
      | isOk && keys `Set.isSubsetOf` okFields =
          genericParseJSON defaultOptions $
            Object $ addConstructorTag okConstructorProxy obj

      | isNonOk && keys `Set.isSubsetOf` failureFields =
          genericParseJSON defaultOptions $
            Object $ addConstructorTag failureConstructorProxy obj

      | otherwise = mismatch

    mismatch :: Parser a -- Making it polymorphic
    mismatch = typeMismatch (typeName (Proxy :: Proxy t)) src

    okConstructorProxy =
      Proxy :: Proxy '(t, "EGAddVinResponseResponsesOk")

    failureConstructorProxy =
      Proxy :: Proxy '(t, "EGAddVinResponseResponsesFailure")

    acceptCodeKey = fieldName (Proxy :: Proxy '(t, "acceptCode"))

    -- | Proving by type-constraint that "vin" field is provided only for
    -- successful case.
    vinKey
      :: ( ConstructorFieldName typeRep
             "EGAddVinResponseResponsesOk" "vin" ~ 'Just "vin"
         , ConstructorFieldName typeRep
             "EGAddVinResponseResponsesFailure" "vin" ~ 'Nothing
         )
      => Text
    vinKey
      = constructorFieldName
      $ proxyPair2Triplet okConstructorProxy (Proxy :: Proxy "vin")

    statusDescriptionKey = fieldName (Proxy :: Proxy '(t, "statusDescription"))

    keys, okFields, failureFields :: Set.Set Text
    keys          = Set.fromList $ HM.keys obj
    okFields      = [acceptCodeKey, statusDescriptionKey, vinKey]
    failureFields = [acceptCodeKey, statusDescriptionKey]

    parsedAcceptCode = fromJSON <$> HM.lookup acceptCodeKey obj
    isOk = parsedAcceptCode == Just (Success OK)

    isNonOk =
      case parsedAcceptCode of
           Just (Success x) -> x /= OK
           _ -> False

  parseJSON invalid = typeMismatch (typeName (Proxy :: Proxy t)) invalid

instance ToSchema EGAddVinResponseResponses where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  declareNamedSchema
    :: forall proxy t. t ~ EGAddVinResponseResponses
    => proxy t
    -> Declare (Definitions Schema) NamedSchema

  declareNamedSchema _ = do
    constructors <-
      typeSafeSchemaMapConstructorsAsProperties
        (Proxy :: Proxy t) constructorMapFn

    pure
      $ NamedSchema (Just typeName'') constructorsBranchingSchemaProto
      { _schemaDiscriminator = Just acceptCodeKey

      , _schemaDescription =
          Just [qms| "{vinKey :: Text}" is added only when
                     "{acceptCodeKey}" is "{toStringy OK}". |]

      , _schemaProperties = fromList constructors
      }

    where
      typeName'' = typeName (Proxy :: Proxy t)

      okConstructorProxy = Proxy :: Proxy '(t, "EGAddVinResponseResponsesOk")
      acceptCodeKey = fieldName (Proxy :: Proxy '(t, "acceptCode"))

      acceptCodeSchema = Inline $ mempty
        { _schemaParamSchema = mempty
            { _paramSchemaType = SwaggerString
            , _paramSchemaEnum = Just $ String . toStringy <$> [OK]
            }
        }

      constructorMapFn constructor scheme
        | constructor == constructorName okConstructorProxy = scheme
            { _schemaProperties =
                fromList [(acceptCodeKey, acceptCodeSchema)]
                  <> _schemaProperties scheme
            , _schemaRequired = acceptCodeKey : _schemaRequired scheme
            }
        | otherwise = scheme

      vinKey
        = constructorFieldName
        $ proxyPair2Triplet okConstructorProxy (Proxy :: Proxy "vin")
