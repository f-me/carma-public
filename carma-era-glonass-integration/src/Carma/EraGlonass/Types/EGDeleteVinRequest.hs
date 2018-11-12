{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings, OverloadedLists, QuasiQuotes #-}

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
import           GHC.TypeLits (symbolVal)

import           Data.Proxy
import           Data.Text (Text)
import           Data.String (fromString)
import           Text.InterpolatedString.QM
import qualified Data.HashMap.Lazy as HM
import qualified Data.Set as Set
import           Data.Aeson
import           Data.Aeson.TH (Options (omitNothingFields))
import           Data.Aeson.Types (typeMismatch, parseEither)
import           Data.Swagger
import           Data.Swagger.Internal.Schema

import           Carma.EraGlonass.Types.Helpers (toStringy, ReplaceFieldKey)
import           Carma.EraGlonass.Types.RequestId (RequestId)
import           Carma.EraGlonass.Types.EGVin (EGVin)
import           Carma.EraGlonass.Types.EGVinOperationAcceptCode
                   ( EGVinOperationAcceptCode (OK, IncorrectFormat, VinNotFound)
                   , EGVinOperationDeletionIsSucceededAcceptCode
                   )


-- *** Request ***


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
  declareNamedSchema _ = gdeclareNamedSchema defaultSchemaOptions proxy mempty
    where proxy :: Proxy (UppercaseVinField (Rep EGDeleteVinRequestRequests))
          proxy = Proxy

type family UppercaseVinField (k1 :: * -> *) where
  UppercaseVinField (D1 a (C1 x field)) =
    D1 a (C1 x (ReplaceFieldKey "vin" VinFieldName field))

type VinFieldName = "VIN"


-- *** Response ***


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
  parseJSON src = pure $
    -- Parsing here to extract parsing error message
    case parseEither (const successfulCase) src of
         Left msg -> EGDeleteVinResponseIncorrect msg src
         Right x  -> x

    where
      successfulCase = do
        obj <- -- Extracting hash-map from JSON @Object@
          case src of
               Object x -> pure x
               _        -> typeMismatch "EGDeleteVinResponse" src

        parsed <- genericParseJSON defaultOptions $
          -- Associating it with successful case constructor
          Object $ HM.insert "tag" (String "EGDeleteVinResponse") obj

        pure parsed

type FailureConsMeta
   = 'MetaCons "EGDeleteVinResponseIncorrect" 'PrefixI 'True

-- | Slicing 'EGDeleteVinResponseIncorrect' constructor from Swagger spec.
type family CutOffFailureCons (k1 :: * -> *) where
  CutOffFailureCons (D1 a (C1 FailureConsMeta _ :+: y)) = D1 a y

instance ToSchema EGDeleteVinResponse where
  declareNamedSchema _ = gdeclareNamedSchema defaultSchemaOptions proxy mempty
    where proxy :: Proxy (CutOffFailureCons (Rep EGDeleteVinResponse))
          proxy = Proxy


data EGDeleteVinResponseResponses
   = EGDeleteVinResponseResponsesSuccess
   -- ^ When @acceptCode@ is either @\"OK"@ or @\"VIN_NOT_FOUND"@
   { acceptCode :: EGVinOperationDeletionIsSucceededAcceptCode
   , statusDescription :: Maybe Text
   , vin :: EGVin
   }

   | EGDeleteVinResponseResponsesIncorrectFormat
   -- ^ When @acceptCode@ is @\"INCORRECT_FORMAT"@
   { statusDescription :: Maybe Text
   }

     deriving (Eq, Show, Generic)

instance FromJSON EGDeleteVinResponseResponses where
  parseJSON src@(Object obj) = go where
    go =
      if acceptCodeKey `Set.member` keys
         then branching
         else typeMismatch typeName src

    branching
      | isSuccess && keys `Set.isSubsetOf` successFields =
          genericParseJSON defaultOptions $
            Object $ HM.insert "tag" (String successConstructor) obj

      | isIncorrectFormat && keys `Set.isSubsetOf` incorrectFormatFields =
          genericParseJSON defaultOptions $
            Object $ HM.insert "tag" (String incorrectFormatConstructor) obj

      | otherwise = typeMismatch typeName src

    typeName                   = "EGDeleteVinResponseResponses"
    successConstructor         = "EGDeleteVinResponseResponsesSuccess"
    incorrectFormatConstructor = "EGDeleteVinResponseResponsesIncorrectFormat"

    acceptCodeKey        = "acceptCode"
    statusDescriptionKey = "statusDescription"
    vinKey               = "vin"

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

  parseJSON invalid = typeMismatch "EGDeleteVinResponseResponses" invalid

instance ToSchema EGDeleteVinResponseResponses where
  declareNamedSchema _ = do
    successAcceptCodeRef <- declareSchemaRef $ unwrapperToProxy acceptCode

    incorrectFormatAcceptCodeRef <-
      pure $ Inline $ mempty
        { _schemaParamSchema = mempty
            { _paramSchemaType = SwaggerString
            , _paramSchemaEnum = Just $ String . toStringy <$> [IncorrectFormat]
            }
        }

    statusDescriptionRef <-
      declareSchemaRef $ unwrapperToProxy statusDescription

    vinRef <- declareSchemaRef $ unwrapperToProxy vin

    successConstructorRef <-
      pure $ Inline $ mempty
        { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }

        , _schemaProperties =
            [ ("acceptCode",        successAcceptCodeRef)
            , ("statusDescription", statusDescriptionRef)
            , ("vin",               vinRef)
            ]

        , _schemaRequired =
            [ "acceptCode"
            , "vin"
            ]
        }

    incorrectFormatConstructorRef <-
      pure $ Inline $ mempty
        { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }

        , _schemaProperties =
            [ ("acceptCode",        incorrectFormatAcceptCodeRef)
            , ("statusDescription", statusDescriptionRef)
            ]

        , _schemaRequired =
            [ "acceptCode"
            ]
        }

    pure
      $ NamedSchema (Just "EGDeleteVinResponseResponses") mempty
      { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
      , _schemaDiscriminator = Just "acceptCode"

      , _schemaDescription =
          Just [qns| "vin" is added only when "acceptCode" is either
                     "OK" or "VIN_NOT_FOUND" (both cases means success). |]

      , _schemaProperties =
          [ ("EGDeleteVinResponseResponsesSuccess", successConstructorRef)
          , ( "EGDeleteVinResponseResponsesIncorrectFormat"
            , incorrectFormatConstructorRef
            )
          ]

      , _schemaMinProperties = Just 1
      , _schemaMaxProperties = Just 1
      }

    where
      unwrapperToProxy :: (EGDeleteVinResponseResponses -> b) -> Proxy b
      unwrapperToProxy _ = Proxy
