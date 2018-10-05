{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
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

import           Data.Proxy
import           Data.Text (Text)
import           Text.InterpolatedString.QM
import qualified Data.HashMap.Lazy as HM
import           Data.Aeson
import           Data.Aeson.TH (Options (omitNothingFields))
import           Data.Aeson.Types (typeMismatch, parseEither)
import           Data.Swagger
import           Data.Swagger.Internal.Schema

import           Carma.Utils.Operators
import           Carma.EraGlonass.Types.RequestId (RequestId)
import           Carma.EraGlonass.Types.EGVin (EGVin)
import           Carma.EraGlonass.Types.EGAcceptCode (EGAcceptCode)


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
   } deriving (Eq, Show, Generic, ToSchema)

instance ToJSON EGAddVinRequestRequests where
  toJSON
    = genericToJSON defaultOptions { omitNothingFields = True }
    ? renameVinField
    where
      -- | In JSON in supposed to be uppercase.
      vinKeyAlias = ("vin", "VIN") :: (Text, Text)

      renameVinField :: Value -> Value

      renameVinField (Object kv) =
        case HM.lookup (fst vinKeyAlias) kv of
             Just x ->
               Object $ kv
                 & HM.delete (fst vinKeyAlias)
                 & HM.insert (snd vinKeyAlias) x
             Nothing ->
               error [qms| Property by key "{fst vinKeyAlias}"
                           not found in hash-map: {kv} |]

      renameVinField x = error [qm| Unexpected JSON type: {x} |]


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
  parseJSON src = pure $
    -- Parsing here to extract parsing error message
    case parseEither (const successfulCase) src of
         Left msg -> EGAddVinResponseIncorrect msg src
         Right x  -> x

    where
      successfulCase = do
        obj <- -- Extracting hash-map from JSON @Object@
          case src of
               Object x -> pure x
               _        -> typeMismatch "EGAddVinResponse" src

        parsed <- genericParseJSON defaultOptions $
          -- Associating it with successful case constructor
          Object $ HM.insert "tag" (String "EGAddVinResponse") obj

        pure parsed

type FailureConsMeta
   = 'MetaCons "EGAddVinResponseIncorrect" 'PrefixI 'True

-- | Slicing 'EGAddVinResponseIncorrect' constructor from Swagger spec.
type family CutOffFailureCons (k1 :: * -> *) where
  CutOffFailureCons (D1 a (C1 FailureConsMeta _ :+: y)) = D1 a y

instance ToSchema EGAddVinResponse where
  declareNamedSchema _ = gdeclareNamedSchema defaultSchemaOptions proxy mempty
    where proxy
            :: (x ~ CutOffFailureCons (Rep EGAddVinResponse))
            => Proxy x
          proxy = Proxy


data EGAddVinResponseResponses
   = EGAddVinResponseResponses
   { acceptCode :: EGAcceptCode -- ^ TODO FIXME add "VIN_NOT_FOUND"
   , statusDescription :: Maybe Text
   , vin :: Maybe EGVin
   } deriving (Eq, Show, Generic, FromJSON, ToSchema)
