{-# LANGUAGE OverloadedStrings, QuasiQuotes, TypeFamilies #-}

-- | General helpers for CaRMa Era Glonass integration microservice.
module Carma.EraGlonass.Helpers
     ( checkRequestsAndResponsesCountsEquality
     , checkConstructredRequestsAndEntitiesCountsEquality
     ) where

import           Text.InterpolatedString.QM

import           Carma.Monad
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Model.CaseEraGlonassFailure.Types
                   (EGIntegrationPoint)


-- | Helper to check if responses count matches requests count
-- otherwise logging an error and throwing an exception.
checkRequestsAndResponsesCountsEquality
  :: ( Eq n, Show n, requestsCount ~ n, responsesCount ~ n
     , ShowQ actionLog
     , ShowQ requestsLogName
     , ShowQ responsesLogName
     , MonadLoggerBus m
     )
  => EGIntegrationPoint
  -> actionLog
  -> (requestsCount,  requestsLogName)
  -> (responsesCount, responsesLogName)
  -> m ()

checkRequestsAndResponsesCountsEquality integrationPoint actionLog
                                        requests         responses
  | requestsCount == responsesCount = pure ()
  | otherwise = do
      logError [qm| {integrationPoint} is failed due to: {failMsg} |]
      fail failMsg
  where
    (requestsCount,  requestsLogName)  = requests
    (responsesCount, responsesLogName) = responses

    failMsg = [qmb|
      Incorrect response to a request {actionLog}.
      Unexpectedly count of responses ({responsesLogName}) \
      is not equal to count of requests ({requestsLogName}).
      \  Count of responses ({responsesLogName}) is {responsesCount}.
      \  Count of requests ({requestsLogName}) is {requestsCount}.
    |]


checkConstructredRequestsAndEntitiesCountsEquality
  :: ( Eq n, Show n, requestsCount ~ n, entitiesCount ~ n
     , ShowQ actionLog
     , ShowQ requestsLogName
     , ShowQ entityLogName
     , MonadLoggerBus m
     )
  => EGIntegrationPoint
  -> actionLog
  -> (requestsCount, requestsLogName)
  -> (entitiesCount, entityLogName)
  -> m ()

checkConstructredRequestsAndEntitiesCountsEquality integrationPoint actionLog
                                                   requests         entities
  | requestsCount == entitiesCount = pure ()
  | otherwise = do
      logError [qm| {integrationPoint} is failed due to: {failMsg} |]
      fail failMsg
  where
    (requestsCount, requestsLogName) = requests
    (entitiesCount, entityLogName)   = entities

    failMsg = [qmb|
      Constructing request {actionLog} is failed.
      Unexpectedly count of "{entityLogName}"s
      is not equal to count of requests ({requestsLogName}).
      \  Count of "{entityLogName}"s is {entitiesCount}.
      \  Count of requests ({requestsLogName}) is {requestsCount}.
    |]
