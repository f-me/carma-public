{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.HashMap.Lazy as HM
import           Data.Aeson
import           Data.Aeson.TH (Options (omitNothingFields))
import           Data.Aeson.Types (typeMismatch, parseEither)
import           Data.Swagger
import           Data.Swagger.Internal.Schema

import           Carma.EraGlonass.Types.Helpers (ReplaceFieldKey)
import           Carma.EraGlonass.Types.RequestId (RequestId)
import           Carma.EraGlonass.Types.EGVin (EGVin)
import           Carma.EraGlonass.Types.EGVinOperationAcceptCode
                   ( EGVinOperationAcceptCode
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
    where proxy
            :: (x ~ CutOffFailureCons (Rep EGDeleteVinResponse))
            => Proxy x
          proxy = Proxy


data EGDeleteVinResponseResponses
   = EGDeleteVinResponseResponses
   { acceptCode :: EGVinOperationAcceptCode
   , statusDescription :: Maybe Text
   , vin :: Maybe EGVin
   } deriving (Eq, Show, Generic, FromJSON, ToSchema)
