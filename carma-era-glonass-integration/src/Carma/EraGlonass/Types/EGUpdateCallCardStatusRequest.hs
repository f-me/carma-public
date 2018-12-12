{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}

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
import           Data.Aeson
import           Data.Aeson.TH (Options (omitNothingFields))
import           Data.Aeson.Types (Parser, typeMismatch, parseEither)
import           Data.Swagger
import           Data.Swagger.Declare (Declare)
import           Data.Swagger.Internal.Schema

import           Carma.Utils.TypeSafe.Generic.DataType
import           Carma.Utils.TypeSafe.Generic.Aeson
import           Carma.EraGlonass.Types.RequestId (RequestId)
import           Carma.EraGlonass.Types.EGCallCardId (EGCallCardId)
import           Carma.EraGlonass.Types.EGCallCardStatus (EGCallCardStatus)
import           Carma.EraGlonass.Types.EGUpdateCallCardAcceptCode
                   ( EGUpdateCallCardAcceptCode
                   )


-- *** Request


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


-- *** Response


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
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  parseJSON
    :: forall t. t ~ EGUpdateCallCardStatusResponse => Value -> Parser t

  parseJSON src = pure $
    -- Parsing here to extract parsing error message
    case parseEither (const successfulCase) src of
         Left msg -> EGUpdateCallCardStatusResponseIncorrect msg src
         Right x  -> x

    where
      typeName'' = typeName (Proxy :: Proxy t)

      okConstructorProxy :: Proxy '(t, "EGUpdateCallCardStatusResponse")
      okConstructorProxy = Proxy

      successfulCase = do
        obj <- -- Extracting hash-map from JSON @Object@
          case src of
               Object x -> pure x
               _        -> typeMismatch typeName'' src

        parsed <- genericParseJSON defaultOptions $
          -- Associating it with successful case constructor
          Object $ addConstructorTag okConstructorProxy obj

        pure parsed

type FailureConsMeta
   = 'MetaCons "EGUpdateCallCardStatusResponseIncorrect" 'PrefixI 'True

-- | Slicing 'EGUpdateCallCardStatusResponseIncorrect' constructor
-- from Swagger spec.
type family CutOffFailureCons (k1 :: * -> *) where
  CutOffFailureCons (D1 a (C1 FailureConsMeta _ :+: y)) = D1 a y

instance ToSchema EGUpdateCallCardStatusResponse where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  declareNamedSchema
    :: forall proxy t typeRep t2
    .  ( t ~ EGUpdateCallCardStatusResponse
       , typeRep ~ Rep t
       , t2 ~ CutOffFailureCons typeRep
       )
    => proxy t
    -> Declare (Definitions Schema) NamedSchema

  declareNamedSchema _ =
    gdeclareNamedSchema defaultSchemaOptions (Proxy :: Proxy t2) mempty


data EGUpdateCallCardStatusResponseResponses
   = EGUpdateCallCardStatusResponseResponses
   { acceptId :: Text
   , acceptCode :: EGUpdateCallCardAcceptCode
   , statusDescription :: Maybe Text
   } deriving (Eq, Show, Generic, FromJSON, ToSchema)
