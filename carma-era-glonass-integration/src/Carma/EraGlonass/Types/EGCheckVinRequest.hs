{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- Fixes issue when record-fields aren't exported. Probably related to:
--   https://stackoverflow.com/questions/46357747/haddock-data-record-fields-names-not-being-generated
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Data-types for checking if VIN is handled by CaRMa (__CRM.EG.02__)
-- request and response.
module Carma.EraGlonass.Types.EGCheckVinRequest
     ( EGCheckVinRequest (..)
     , EGCheckVinRequestRequests (..)
     , EGCheckVinResponse (..)
     , EGCheckVinResponseResponses (..)
     , EGCheckVinResponseVinProviders (..)
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


-- *** Request ***


data EGCheckVinRequest
   = EGCheckVinRequest
   { requestId :: RequestId
   , requests :: [EGCheckVinRequestRequests]
   } deriving (Eq, Show, Generic, ToSchema)

instance ToJSON EGCheckVinRequest where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }


data EGCheckVinRequestRequests
   = EGCheckVinRequestRequests
   { vin :: EGVin
   } deriving (Eq, Show, Generic, ToSchema)

instance ToJSON EGCheckVinRequestRequests where
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


data EGCheckVinResponse
   = EGCheckVinResponseIncorrect
   { errorMessage :: String
   , incorrectResponseBody :: Value
   }

   | EGCheckVinResponse
   { requestId :: RequestId
   , responses :: [EGCheckVinResponseResponses]
   }
     deriving (Eq, Show, Generic)

instance FromJSON EGCheckVinResponse where
  parseJSON src = pure $
    -- Parsing here to extract parsing error message
    case parseEither (const successfulCase) src of
         Left msg -> EGCheckVinResponseIncorrect msg src
         Right x  -> x

    where
      successfulCase = do
        obj <- -- Extracting hash-map from JSON @Object@
          case src of
               Object x -> pure x
               _        -> typeMismatch "EGCheckVinResponse" src

        parsed <- genericParseJSON defaultOptions $
          -- Associating it with successful case constructor
          Object $ HM.insert "tag" (String "EGCheckVinResponse") obj

        pure parsed

type FailureConsMeta
   = 'MetaCons "EGCheckVinResponseIncorrect" 'PrefixI 'True

-- | Slicing 'EGCheckVinResponseIncorrect' constructor from Swagger spec.
type family CutOffFailureCons (k1 :: * -> *) where
  CutOffFailureCons (D1 a (C1 FailureConsMeta _ :+: y)) = D1 a y

instance ToSchema EGCheckVinResponse where
  declareNamedSchema _ = gdeclareNamedSchema defaultSchemaOptions proxy mempty
    where proxy
            :: (x ~ CutOffFailureCons (Rep EGCheckVinResponse))
            => Proxy x
          proxy = Proxy


data EGCheckVinResponseResponses
   = EGCheckVinResponseResponses
   { vinStatus :: Bool
   , vin :: EGVin
   , vinProviders :: Maybe [EGCheckVinResponseVinProviders]
   } deriving (Eq, Show, Generic, FromJSON, ToSchema)


data EGCheckVinResponseVinProviders
   = EGCheckVinResponseVinProviders
   { code :: Text
   , title :: Text
   } deriving (Eq, Show, Generic, FromJSON, ToSchema)
