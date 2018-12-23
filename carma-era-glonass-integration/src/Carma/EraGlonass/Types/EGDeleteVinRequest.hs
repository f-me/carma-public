{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists, QuasiQuotes #-}
{-# LANGUAGE InstanceSigs #-}

-- Fixes issue when record-fields aren't exported. Probably related to:
--   https://stackoverflow.com/questions/46357747/haddock-data-record-fields-names-not-being-generated
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Data-types for deleting VIN from list of VINs handled by CaRMa
-- (__CRM.EG.02__), request and response.
module Carma.EraGlonass.Types.EGDeleteVinRequest
     ( EGDeleteVinRequest (..)
     , EGDeleteVinRequestRequests (..)
     , EGDeleteVinResponse (..)
     , EGDeleteVinResponseResponses (..)
     ) where

import           GHC.Generics
import           GHC.TypeLits
import           GHC.Exts (IsList (..))

import           Data.Proxy
import           Data.Monoid
import           Data.Text (Text)
import           Data.String (fromString)
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
                   ( EGVinOperationAcceptCode (OK, IncorrectFormat, VinNotFound)
                   , EGVinOperationDeletionIsSucceededAcceptCode
                   )


-- *** Request


data EGDeleteVinRequest
   = EGDeleteVinRequest
   { requestId :: RequestId
   , requests :: [EGDeleteVinRequestRequests]
   } deriving (Eq, Show, Generic, ToSchema)

instance ToJSON EGDeleteVinRequest where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }


data EGDeleteVinRequestRequests
   = EGDeleteVinRequestRequests
   { vin :: EGVin
   } deriving (Eq, Show, Generic)

instance ToJSON EGDeleteVinRequestRequests where
  toJSON (EGDeleteVinRequestRequests (vin' :: EGVin)) =
    object [fromString (symbolVal (Proxy :: Proxy VinFieldName)) .= vin']

instance ToSchema EGDeleteVinRequestRequests where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  declareNamedSchema
    :: forall proxy t t2
    .  ( t ~ EGDeleteVinRequestRequests
       , t2 ~ UppercaseVinField (Rep t)
       )
    => proxy t
    -> Declare (Definitions Schema) NamedSchema

  declareNamedSchema _ =
    gdeclareNamedSchema defaultSchemaOptions (Proxy :: Proxy t2) mempty

type family UppercaseVinField (k1 :: * -> *) where
  UppercaseVinField (D1 a (C1 x field)) =
    D1 a (C1 x (ReplaceFieldKey "vin" VinFieldName field))

type VinFieldName = "VIN"


-- *** Response


data EGDeleteVinResponse
   = EGDeleteVinResponseIncorrect
   { errorMessage :: String
   , incorrectResponseBody :: Value
   }

   | EGDeleteVinResponse
   { requestId :: RequestId
   , responses :: [EGDeleteVinResponseResponses]
   }
     deriving (Eq, Show, Generic)

instance FromJSON EGDeleteVinResponse where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  parseJSON :: forall t. t ~ EGDeleteVinResponse => Value -> Parser t
  parseJSON src = pure $
    -- Parsing here to extract parsing error message
    case parseEither (const successfulCase) src of
         Left msg -> EGDeleteVinResponseIncorrect msg src
         Right x  -> x

    where
      typeName'' = typeName (Proxy :: Proxy t)

      okConstructorProxy :: Proxy '(t, "EGDeleteVinResponse")
      okConstructorProxy = Proxy

      successfulCase = do
        obj <- -- Extracting hash-map from JSON @Object@
          case src of
               Object x -> pure x
               _        -> typeMismatch typeName'' src

        genericParseJSON defaultOptions $
          -- Associating it with successful case constructor
          Object $ addConstructorTag okConstructorProxy obj

type FailureConsMeta
   = 'MetaCons "EGDeleteVinResponseIncorrect" 'PrefixI 'True

-- | Slicing 'EGDeleteVinResponseIncorrect' constructor from Swagger spec.
type family CutOffFailureCons (k1 :: * -> *) where
  CutOffFailureCons (D1 a (C1 FailureConsMeta _ :+: y)) = D1 a y

instance ToSchema EGDeleteVinResponse where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  declareNamedSchema
    :: forall proxy t t2
    .  ( t ~ EGDeleteVinResponse
       , t2 ~ CutOffFailureCons (Rep t)
       )
    => proxy t
    -> Declare (Definitions Schema) NamedSchema

  declareNamedSchema _ =
    gdeclareNamedSchema defaultSchemaOptions (Proxy :: Proxy t2) mempty


-- | List item type of "responses" field of "EGDeleteVinResponse"
data EGDeleteVinResponseResponses
   = EGDeleteVinResponseResponsesSuccess
   { acceptCode :: EGVinOperationDeletionIsSucceededAcceptCode
   , statusDescription :: Maybe Text
   , vin :: EGVin
   } -- ^ When @acceptCode@ is either @\"OK"@ or @\"VIN_NOT_FOUND"@

   | EGDeleteVinResponseResponsesIncorrectFormat
   { statusDescription :: Maybe Text
   } -- ^ When @acceptCode@ is @\"INCORRECT_FORMAT"@

     deriving (Eq, Show, Generic)

instance FromJSON EGDeleteVinResponseResponses where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  parseJSON :: forall t. t ~ EGDeleteVinResponseResponses => Value -> Parser t
  parseJSON src@(Object obj) = go where
    go = if Set.member acceptCodeKey keys then branching else mismatch

    branching
      | isSuccess && keys `Set.isSubsetOf` successFields =
          genericParseJSON defaultOptions $
            Object $ addConstructorTag successConstructorProxy obj

      | isIncorrectFormat && keys `Set.isSubsetOf` incorrectFormatFields =
          genericParseJSON defaultOptions $
            Object $ addConstructorTag incorrectFormatConstructorProxy obj

      | otherwise = mismatch

    mismatch :: Parser a -- Making it polymorphic
    mismatch = typeMismatch (typeName (Proxy :: Proxy t)) src

    successConstructorProxy =
      Proxy :: Proxy '(t, "EGDeleteVinResponseResponsesSuccess")

    incorrectFormatConstructorProxy =
      Proxy :: Proxy '(t, "EGDeleteVinResponseResponsesIncorrectFormat")

    acceptCodeKey
      = constructorFieldName
      $ proxyPair2Triplet successConstructorProxy (Proxy :: Proxy "acceptCode")

    statusDescriptionKey = fieldName (Proxy :: Proxy '(t, "statusDescription"))

    vinKey
      = constructorFieldName
      $ proxyPair2Triplet successConstructorProxy (Proxy :: Proxy "vin")

    keys, successFields, incorrectFormatFields :: Set.Set Text
    keys                  = Set.fromList $ HM.keys obj
    successFields         = [acceptCodeKey, statusDescriptionKey, vinKey]
    incorrectFormatFields = [acceptCodeKey, statusDescriptionKey]

    parsedAcceptCode = fromJSON <$> HM.lookup acceptCodeKey obj
    isIncorrectFormat = parsedAcceptCode == Just (Success IncorrectFormat)

    isSuccess =
      case parsedAcceptCode of
           Just (Success x) -> x == OK || x == VinNotFound
           _ -> False

  parseJSON invalid = typeMismatch (typeName (Proxy :: Proxy t)) invalid

instance ToSchema EGDeleteVinResponseResponses where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  declareNamedSchema
    :: forall proxy t. t ~ EGDeleteVinResponseResponses
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
          Just [qms| "{vinKey :: Text}" is added only when "{acceptCodeKey}" is
                     either "{toStringy OK}" or "{toStringy VinNotFound}"
                     (both cases means success). |]

      , _schemaProperties = fromList constructors
      }

    where
      typeName'' = typeName (Proxy :: Proxy t)

      successConstructorProxy =
        Proxy :: Proxy '(t, "EGDeleteVinResponseResponsesSuccess")

      incorrectFormatConstructorProxy =
        Proxy :: Proxy '(t, "EGDeleteVinResponseResponsesIncorrectFormat")

      acceptCodeKey = fieldName (Proxy :: Proxy '(t, "acceptCode"))

      vinKey
        = constructorFieldName
        $ proxyPair2Triplet successConstructorProxy (Proxy :: Proxy "vin")

      incorrectFormatAcceptCodeSchema = Inline $ mempty
        { _schemaParamSchema = mempty
            { _paramSchemaType = SwaggerString
            , _paramSchemaEnum = Just $ String . toStringy <$> [IncorrectFormat]
            }
        }

      constructorMapFn constructor scheme
        | constructor == constructorName incorrectFormatConstructorProxy =
            scheme
              { _schemaProperties =
                  fromList [(acceptCodeKey, incorrectFormatAcceptCodeSchema)]
                    <> _schemaProperties scheme
              , _schemaRequired = acceptCodeKey : _schemaRequired scheme
              }
        | otherwise = scheme
