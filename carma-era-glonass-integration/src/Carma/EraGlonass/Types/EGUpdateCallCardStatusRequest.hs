{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}

-- Fixes issue when record-fields aren't exported. Probably related to:
--   https://stackoverflow.com/questions/46357747/haddock-data-record-fields-names-not-being-generated
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Data-types for __CRM.EG.03__ request and response.
module Carma.EraGlonass.Types.EGUpdateCallCardStatusRequest
     ( EGUpdateCallCardStatusRequest (..)
     , EGUpdateCallCardStatusRequestRequests (..)
     , EGUpdateCallCardStatusRequestStatus (..)
     , EGUpdateCallCardStatusResponse (..)
     , EGUpdateCallCardStatusResponseResponses (..)
     ) where

import           GHC.Generics

import           Data.Proxy
import           Data.Word
import           Data.Text (Text)
import qualified Data.HashMap.Lazy as HM
import           Data.Aeson
import           Data.Aeson.TH (Options (omitNothingFields))
import           Data.Aeson.Types (typeMismatch, parseEither)
import           Data.Swagger
import           Data.Swagger.Internal.Schema

import           Carma.EraGlonass.Types.RequestId (RequestId)
import           Carma.EraGlonass.Types.EGCallCardId (EGCallCardId)
import           Carma.EraGlonass.Types.EGAcceptCode (EGAcceptCode)
import           Carma.EraGlonass.Types.EGCallCardStatus (EGCallCardStatus)


data EGUpdateCallCardStatusRequest
   = EGUpdateCallCardStatusRequest
   { requestId :: RequestId
   , requests :: [EGUpdateCallCardStatusRequestRequests]
   } deriving (Eq, Show, Generic, ToSchema)

instance ToJSON EGUpdateCallCardStatusRequest where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }


data EGUpdateCallCardStatusRequestRequests
   = EGUpdateCallCardStatusRequestRequests
   { cardIdCC :: EGCallCardId
   , cardStatusProvider :: EGUpdateCallCardStatusRequestStatus
   , statusChangeTimestamp :: Word64
   , comment :: Maybe Text -- ^ Any comment from CaRMa
   } deriving (Eq, Show, Generic, ToSchema)

instance ToJSON EGUpdateCallCardStatusRequestRequests where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }


data EGUpdateCallCardStatusRequestStatus
   = EGUpdateCallCardStatusRequestStatus
   { code :: EGCallCardStatus
   , title :: Maybe Text
   } deriving (Eq, Show, Generic, ToSchema)

instance ToJSON EGUpdateCallCardStatusRequestStatus where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }


data EGUpdateCallCardStatusResponse
   = EGUpdateCallCardStatusResponseIncorrect
   { errorMessage :: String
   , incorrectResponseBody :: Value
   }

   | EGUpdateCallCardStatusResponse
   { requestId :: RequestId
   , responses :: [EGUpdateCallCardStatusResponseResponses]
   }

     deriving (Eq, Show, Generic)

instance FromJSON EGUpdateCallCardStatusResponse where
  parseJSON src = pure $
    -- Parsing here to extract parsing error message
    case parseEither (const successfulCase) src of
         Left msg -> EGUpdateCallCardStatusResponseIncorrect msg src
         Right x  -> x

    where
      successfulCase = do
        obj <- -- Extracting hash-map from JSON @Object@
          case src of
               Object x -> pure x
               _        -> typeMismatch "EGUpdateCallCardStatusResponse" src

        parsed <- genericParseJSON defaultOptions $
          -- Associating it with successful case constructor
          Object $ HM.insert "tag" (String "EGUpdateCallCardStatusResponse") obj

        pure parsed

type FailureConsMeta
   = 'MetaCons "EGUpdateCallCardStatusResponseIncorrect" 'PrefixI 'True

-- | Slicing 'EGUpdateCallCardStatusResponseIncorrect' constructor
-- from Swagger spec.
type family CutOffFailureCons (k1 :: * -> *) where
  CutOffFailureCons (D1 a (C1 FailureConsMeta _ :+: y)) = D1 a y

instance ToSchema EGUpdateCallCardStatusResponse where
  declareNamedSchema _ = gdeclareNamedSchema defaultSchemaOptions proxy mempty
    where proxy
            :: (x ~ CutOffFailureCons (Rep EGUpdateCallCardStatusResponse))
            => Proxy x
          proxy = Proxy


data EGUpdateCallCardStatusResponseResponses
   = EGUpdateCallCardStatusResponseResponses
   { acceptId :: Text
   , acceptCode :: EGAcceptCode -- TODO FIXME add "CARD_NOT_FOUND" code
   , statusDescription :: Maybe Text
   } deriving (Eq, Show, Generic, FromJSON, ToSchema)
