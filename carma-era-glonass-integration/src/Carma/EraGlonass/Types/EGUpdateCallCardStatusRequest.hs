{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Data-types for __CRM.EG.03__ request and response.
module Carma.EraGlonass.Types.EGUpdateCallCardStatusRequest
     ( EGUpdateCallCardStatusRequest (..)
     , EGUpdateCallCardStatusRequestRequests (..)
     , EGUpdateCallCardStatusRequestStatus (..)
     , EGUpdateCallCardStatusResponse (..)
     , EGUpdateCallCardStatusResponseResponses (..)
     ) where

import           GHC.Generics

import           Data.Word
import           Data.Text (Text)
import           Data.Aeson

import           Carma.EraGlonass.Types.RequestId (RequestId)
import           Carma.EraGlonass.Types.EGCallCardId (EGCallCardId)
import           Carma.EraGlonass.Types.EGAcceptCode (EGAcceptCode)
import           Carma.EraGlonass.Types.EGCallCardStatus (EGCallCardStatus)


data EGUpdateCallCardStatusRequest
   = EGUpdateCallCardStatusRequest
   { requestId :: RequestId
   , requests :: [EGUpdateCallCardStatusRequestRequests]
   } deriving (Eq, Show, Generic)


data EGUpdateCallCardStatusRequestRequests
   = EGUpdateCallCardStatusRequestRequests
   { cardIdCC :: EGCallCardId
   , cardStatusProvider :: EGUpdateCallCardStatusRequestStatus
   , statusChangeTimestamp :: Word64
   , comment :: Maybe Text -- ^ Any comment from CaRMa
   } deriving (Eq, Show, Generic)


data EGUpdateCallCardStatusRequestStatus
   = EGUpdateCallCardStatusRequestStatus
   { code :: EGCallCardStatus
   , title :: Maybe Text
   } deriving (Eq, Show, Generic)


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


data EGUpdateCallCardStatusResponseResponses
   = EGUpdateCallCardStatusResponseResponses
   { acceptId :: Text
   , acceptCode :: EGAcceptCode -- TODO FIXME add "CARD_NOT_FOUND" code
   , statusDescription :: Maybe Text
   } deriving (Eq, Show, Generic)
