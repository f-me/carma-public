{-# LANGUAGE DuplicateRecordFields, OverloadedStrings, OverloadedLists #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, ScopedTypeVariables #-}
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
import           GHC.TypeLits (symbolVal)

import           Data.Proxy
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

import           Carma.EraGlonass.Types.Helpers
                   ( ReplaceFieldKey
                   , toStringy
                   , typeName
                   , addConstructorTag
                   , proxyPair
                   )
import           Carma.EraGlonass.Types.RequestId (RequestId)
import           Carma.EraGlonass.Types.EGVin (EGVin)
import           Carma.EraGlonass.Types.EGVinOperationAcceptCode
                   ( EGVinOperationAcceptCode (OK)
                   , EGVinOperationFailureAcceptCode
                   )


-- *** Request ***


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


-- *** Response ***


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
  parseJSON src = pure result where
    result =
      -- Parsing here to extract parsing error message
      case parseEither (const successfulCase) src of
           Left msg -> EGAddVinResponseIncorrect msg src
           Right x  -> x

    typeName' = typeName $ pure result

    okConstructorProxy =
      proxyPair (pure result) (Proxy :: Proxy "EGAddVinResponse")

    successfulCase = do
      obj <- -- Extracting hash-map from JSON @Object@
        case src of
             Object x -> pure x
             _        -> typeMismatch typeName' src

      parsed <- genericParseJSON defaultOptions $
        -- Associating it with successful case constructor
        Object $ addConstructorTag okConstructorProxy obj

      pure parsed

type FailureConsMeta
   = 'MetaCons "EGAddVinResponseIncorrect" 'PrefixI 'True

-- | Slicing 'EGAddVinResponseIncorrect' constructor from Swagger spec.
type family CutOffFailureCons (k1 :: * -> *) where
  CutOffFailureCons (D1 a (C1 FailureConsMeta _ :+: y)) = D1 a y

instance ToSchema EGAddVinResponse where
  declareNamedSchema _ = gdeclareNamedSchema defaultSchemaOptions proxy mempty
    where proxy :: Proxy (CutOffFailureCons (Rep EGAddVinResponse))
          proxy = Proxy


data EGAddVinResponseResponses
   = EGAddVinResponseResponsesOk -- ^ When @acceptCode@ is @\"OK"@
   { statusDescription :: Maybe Text
   , vin :: EGVin
   }

   | EGAddVinResponseResponsesFailure -- ^ When @acceptCode@ is not @\"OK"@
   { acceptCode :: EGVinOperationFailureAcceptCode
   , statusDescription :: Maybe Text
   }

     deriving (Eq, Show, Generic)

instance FromJSON EGAddVinResponseResponses where
  parseJSON src@(Object obj) = go where
    go =
      if acceptCodeKey `Set.member` keys
         then branching
         else typeMismatch typeName' src

    branching
      | isOk && keys `Set.isSubsetOf` okFields =
          genericParseJSON defaultOptions $
            Object $ addConstructorTag okConstructorProxy obj

      | isNonOk && keys `Set.isSubsetOf` failureFields =
          genericParseJSON defaultOptions $
            Object $ addConstructorTag failureConstructorProxy obj

      | otherwise = typeMismatch typeName' src

    typeProxy = f go where f :: Parser t -> Proxy t ; f _ = Proxy
    typeName' = typeName typeProxy

    okConstructorProxy =
      proxyPair typeProxy (Proxy :: Proxy "EGAddVinResponseResponsesOk")

    failureConstructorProxy =
      proxyPair typeProxy (Proxy :: Proxy "EGAddVinResponseResponsesFailure")

    acceptCodeKey        = "acceptCode"
    statusDescriptionKey = "statusDescription"
    vinKey               = "vin"

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

  parseJSON x = go where
    go = typeMismatch (typeName typeProxy) x
    typeProxy = f go where f :: Parser t -> Proxy t ; f _ = Proxy

instance ToSchema EGAddVinResponseResponses where
  declareNamedSchema _ = do
    okAcceptCodeRef <-
      pure $ Inline $ mempty
        { _schemaParamSchema = mempty
            { _paramSchemaType = SwaggerString
            , _paramSchemaEnum = Just $ String . toStringy <$> [OK]
            }
        }

    failureAcceptCodeRef <- declareSchemaRef $ unwrapperToProxy acceptCode

    statusDescriptionRef <-
      declareSchemaRef $ unwrapperToProxy statusDescription

    vinRef <- declareSchemaRef $ unwrapperToProxy vin

    okConstructorRef <-
      pure $ Inline $ mempty
        { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }

        , _schemaProperties =
            [ ("acceptCode",        okAcceptCodeRef)
            , ("statusDescription", statusDescriptionRef)
            , ("vin",               vinRef)
            ]

        , _schemaRequired =
            [ "acceptCode"
            , "vin"
            ]
        }

    failureConstructorRef <-
      pure $ Inline $ mempty
        { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }

        , _schemaProperties =
            [ ("acceptCode",        failureAcceptCodeRef)
            , ("statusDescription", statusDescriptionRef)
            ]

        , _schemaRequired =
            [ "acceptCode"
            ]
        }

    pure
      $ NamedSchema (Just typeName') mempty
      { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
      , _schemaDiscriminator = Just "acceptCode"

      , _schemaDescription =
          Just [qn| "vin" is added only when "acceptCode" is "OK". |]

      , _schemaProperties =
          [ ("EGAddVinResponseResponsesOk",      okConstructorRef)
          , ("EGAddVinResponseResponsesFailure", failureConstructorRef)
          ]

      , _schemaMinProperties = Just 1
      , _schemaMaxProperties = Just 1
      }

    where
      typeName' = typeName (Proxy :: Proxy EGAddVinResponseResponses)

      unwrapperToProxy :: (EGAddVinResponseResponses -> b) -> Proxy b
      unwrapperToProxy _ = Proxy
