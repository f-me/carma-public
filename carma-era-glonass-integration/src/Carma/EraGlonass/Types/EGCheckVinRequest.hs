{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
import           GHC.TypeLits

import           Data.Proxy
import           Data.Text (Text)
import           Data.String (fromString)
import           Data.Aeson
import           Data.Aeson.TH (Options (omitNothingFields))
import           Data.Aeson.Types (Parser, typeMismatch, parseEither)
import           Data.Swagger
import           Data.Swagger.Internal.Schema

import           Carma.EraGlonass.Types.Helpers
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
   } deriving (Eq, Show, Generic)

instance ToJSON EGCheckVinRequestRequests where
  toJSON (EGCheckVinRequestRequests (vin' :: EGVin)) =
    object [fromString (symbolVal (Proxy :: Proxy VinFieldName)) .= vin']

instance ToSchema EGCheckVinRequestRequests where
  declareNamedSchema _ = gdeclareNamedSchema defaultSchemaOptions proxy mempty
    where proxy :: Proxy (UppercaseVinField (Rep EGCheckVinRequestRequests))
          proxy = Proxy

type family UppercaseVinField (k1 :: * -> *) where
  UppercaseVinField (D1 a (C1 x field)) =
    D1 a (C1 x (ReplaceFieldKey "vin" VinFieldName field))

type VinFieldName = "VIN"


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
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  parseJSON :: forall t. (t ~ EGCheckVinResponse) => Value -> Parser t
  parseJSON src = pure $
    -- Parsing here to extract parsing error message
    case parseEither (const successfulCase) src of
         Left msg -> EGCheckVinResponseIncorrect msg src
         Right x  -> x

    where
      typeName'' = typeName (Proxy :: Proxy t)

      okConstructorProxy :: Proxy '(t, "EGCheckVinResponse")
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
   = 'MetaCons "EGCheckVinResponseIncorrect" 'PrefixI 'True

-- | Slicing 'EGCheckVinResponseIncorrect' constructor from Swagger spec.
type family CutOffFailureCons (k1 :: * -> *) where
  CutOffFailureCons (D1 a (C1 FailureConsMeta _ :+: y)) = D1 a y

instance ToSchema EGCheckVinResponse where
  declareNamedSchema _ = gdeclareNamedSchema defaultSchemaOptions proxy mempty
    where proxy :: Proxy (CutOffFailureCons (Rep EGCheckVinResponse))
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
