{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}

module Carma.EraGlonass.VinSynchronizer.UnmarkAsHandled
     ( unmarkAsHandled
     ) where

import           Data.Proxy
import           Data.Typeable
import           Text.InterpolatedString.QM
import qualified Data.Vector as Vec
import           Data.List.NonEmpty (toList)

import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Reader (ReaderT, asks)
import           Control.Monad.Catch

import           Database.Persist.Sql (SqlBackend, fromSqlKey)
import           Database.Persist.Types (Entity (..))

import           Carma.Utils.Operators
import           Carma.Monad
import           Carma.Model.Contract.Persistent
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Types
import           Carma.EraGlonass.Model.CaseEraGlonassFailure.Types
import           Carma.EraGlonass.Model.EraGlonassSynchronizedContract.Persistent
import           Carma.EraGlonass.Client
import           Carma.EraGlonass.VinSynchronizer.Types


unmarkAsHandled
  :: VinSynchronizerMonad m
  => OneOrTwoNonEmptyLists
       (Entity Contract)
       (Entity EraGlonassSynchronizedContract)
  -> ReaderT SqlBackend m ()

unmarkAsHandled lists = do
  -- Obtaining only those which is really handled by us at the moment
  -- reported by Era Glonass side. And later all of them, these and others
  -- will be /unmarked/ on our side (all of them supposed to be, on our
  -- side, those which just /unmarked/ on Era Glonass side, and those which
  -- just not /unmarked/ by us yet by some circumstances).
  (contracts, ephemerals) <- lift $ runConcurrently $ (,)
    <$> Concurrently ( maybe (pure []) (toList ? checkContracts)
                     $ getFirstNonEmptyList lists
                     )
    <*> Concurrently ( maybe (pure []) (toList ? checkEphemeral)
                     $ getSecondNonEmptyList lists
                     )

  logDebug [qm| CHECKPOINT! |] -- TODO
  contracts `seq` ephemerals `seq` pure ()

  where
    checkContracts
      :: VinSynchronizerMonad m
      => [Entity Contract]
      -> m [ContractId]
      -- ^ Returns a list of "Contract"s which are handled by us, so these
      --   we're supposed to /unmark/ (filtered only those who match our own
      --   provider).

    checkContracts entities = do
      logDebug [qms|
        Checking status of CaRMa outdated/deactivated "{contractTypeRep}"s
        on Era Glonass side (checking whether they're handled by us
        so we have to "unmark" them as handled by us)...
      |]

      let -- | @Left@ if some VIN is incorrect.
          requests :: Either String [EGCheckVinRequestRequests]
          requests = Vec.toList <$> foldM reduceFn Vec.empty entities where
            reduceFn acc Entity {..} =
              case contractVin entityVal of
                   Just (textToProofedEGVin -> parsedVin) ->
                     case parsedVin of
                          Right proofedVin ->
                            Right $ Vec.snoc acc
                              EGCheckVinRequestRequests { vin = proofedVin }

                          Left msg -> Left [qms|
                            VIN of a "{contractTypeRep}" with id
                            {fromSqlKey entityKey} is incorrect.
                            Error message: {msg}
                          |]

                   Nothing -> Left [qms|
                     VIN of a "{contractTypeRep}" with id
                     {fromSqlKey entityKey} could not be "Nothing",
                     it supposed to be checked earlier.
                   |]

      requestsList <- case requests of
        Right x -> pure x
        Left  e ->
          logError [qms|
            {CrmEg02} is failed on constructing request to check status of
            CaRMa outdated/deactivated "{contractTypeRep}"s.
            Error: {e}
          |] >> fail e

      -- Checking for data correctness
      uncurry when $
        let
          lenContracts = length entities
          lenRequests  = length requestsList

          failMsg = [qmb|
            Constructing request to check status of \
            CaRMa outdated/deactivated "{contractTypeRep}"s is failed.
            Unexpectedly count of "{contractTypeRep}"s is not equal \
            to count of requests (VINs to check).
            \  Count of "{contractTypeRep}"s is {lenContracts}.
            \  Count of requests (VINs to check) is {lenRequests}.
          |]

          m = do
            logError [qm| {CrmEg02} is failed due to: {failMsg} |]
            fail failMsg
        in
          (lenContracts /= lenRequests, m)

      reqId <- newRequestId

      let egRequestData =
            EGCheckVinRequest
              { requestId = reqId
              , requests  = requestsList
              }

      let -- | Resolver for possible failure of HTTP-request.
          requestResolver (Right x) = pure x
          requestResolver (Left  e) = do
            logError [qms|
              {CrmEg02} is failed on POST request to Era Glonass to check
              status of CaRMa outdated/deactivated "{contractTypeRep}"s.
              Error: {e}
            |]

            throwM $ EGCheckVinRequestIsFailed e egRequestData

      let responseResolver EGCheckVinResponseIncorrect {..} = do
            logError [qmb|
              {CrmEg02} is failed on parsing response data \
              from Era Glonass to check status \
              of CaRMa outdated/deactivated "{contractTypeRep}"s.
              \  Error message: {errorMessage}
              \  Incorrect response body: {incorrectResponseBody}
            |]

            throwM $
              EGCheckVinResponseIsFailed errorMessage incorrectResponseBody

          responseResolver EGCheckVinResponse {..} = do
            ourCode <- asks carmaEgServiceCode

            -- Checking for data correctness
            uncurry when $
              let
                lenRequests  = length requestsList
                lenResponses = length responses

                failMsg = [qmb|
                  Incorrect response to a request to check status of \
                  CaRMa outdated/deactivated "{contractTypeRep}"s.
                  Unexpectedly count of responses (checked VINs) \
                  is not equal to count of requests (VINs to check).
                  \  Count of responses (checked VINs) is {lenResponses}.
                  \  Count of requests (VINs to check) is {lenRequests}.
                |]

                m = do
                  logError [qm| {CrmEg02} is failed due to: {failMsg} |]
                  fail failMsg
              in
                (lenRequests /= lenResponses, m)

            let vinsAreNotEqualFailMsg contractId reqVin resVin = [qmb|
                  Incorrect response to a request to check status of \
                  CaRMa outdated/deactivated "{contractTypeRep}"s.
                  Unexpectedly a VIN from response is not equal to one
                  from request (at the same position in order).
                  \  {contractTypeRep} id is {fromSqlKey contractId}.
                  \  VIN from request is {reqVin}.
                  \  VIN from response is {resVin}.
                |]

            let -- | Reducer which extracts
                reduceFn
                  :: Vec.Vector ContractId
                  -> ( ContractId
                     , EGCheckVinRequestRequests
                     , EGCheckVinResponseResponses
                     )
                  -> Either String (Vec.Vector ContractId)

                reduceFn acc ( contractId
                             , EGCheckVinRequestRequests { vin = reqVin }
                             , EGCheckVinResponseResponsesVinExists
                                 { vin = resVin, vinProviders }
                             ) = do

                  -- Checking for data correctness
                  when (reqVin /= resVin) $
                    Left $ vinsAreNotEqualFailMsg contractId reqVin resVin

                  Right $ if any f vinProviders
                             then Vec.snoc acc contractId
                             else acc -- Already /unmarked/
                  where
                    -- | A predicate which checks that VIN is handled by us
                    f EGCheckVinResponseVinProviders {..} = code == ourCode

                reduceFn acc ( contractId
                             , EGCheckVinRequestRequests { vin = reqVin }
                             , EGCheckVinResponseResponsesVinNotExists
                                 { vin = resVin }
                             ) =
                  if reqVin == resVin -- Checking for data correctness
                     then Right acc -- Already /unmarked/
                     else Left
                        $ vinsAreNotEqualFailMsg contractId reqVin resVin

            let result
                  = fmap Vec.toList
                  $ foldM reduceFn Vec.empty
                  $ zip3 (entityKey <$> entities) requestsList responses

            case result of
                 Right x  -> pure x
                 Left msg -> do
                   logError [qm| {CrmEg02} is failed due to: {msg} |]
                   fail msg

      (asks egClientEnv >>= runClientM (crmEG02Post egRequestData))
        >>= requestResolver
        >>= responseResolver

    checkEphemeral
      :: VinSynchronizerMonad m
      => [Entity EraGlonassSynchronizedContract]
      -> m [EraGlonassSynchronizedContractId]
      -- ^ Returns a list of "EraGlonassSynchronizedContract"s
      --   which are handled by us, so these we're supposed to /unmark/
      --   (filtered only those who match our own provider).

    checkEphemeral entities = do
      logDebug [qns|
        Checking status of CaRMa "ephemeral VINs"
        on Era Glonass side (checking whether they're handled by us
        so we have to "unmark" them as handled by us)...
      |]

      let -- | @Left@ if some VIN is incorrect.
          requests :: Either String [EGCheckVinRequestRequests]
          requests = Vec.toList <$> foldM reduceFn Vec.empty entities where
            appendReq acc x =
              Vec.snoc acc EGCheckVinRequestRequests { vin = x }
            reduceFn acc Entity {..} =
              case ( eraGlonassSynchronizedContractVin entityVal
                   & textToProofedEGVin
                   ) <&> appendReq acc
                of x@(Right _) -> x
                   Left msg -> Left [qms|
                     VIN of a "{eraGlonassSynchronizedContractTypeRep}"
                     with id {fromSqlKey entityKey} is incorrect
                     (which is really strange,
                     it supposed to be validated earlier).
                     Error message: {msg}
                   |]

      requestsList <- case requests of
        Right x -> pure x
        Left  e ->
          logError [qms|
            {CrmEg02} is failed on constructing request to check status of
            CaRMa "ephemeral VINs"
            ("{eraGlonassSynchronizedContractTypeRep}"s).
            Error: {e}
          |] >> fail e

      reqId <- newRequestId

      let egRequestData =
            EGCheckVinRequest
              { requestId = reqId
              , requests  = requestsList
              }

      let -- | Resolver for possible failure of HTTP-request.
          requestResolver (Right x) = pure x
          requestResolver (Left  e) = do
            logError [qms|
              {CrmEg02} is failed on POST request to Era Glonass to check
              status of CaRMa "ephemeral VINs"
              ("{eraGlonassSynchronizedContractTypeRep}"s).
              Error: {e}
            |]

            throwM $ EGCheckVinRequestIsFailed e egRequestData

      let responseResolver EGCheckVinResponseIncorrect {..} = do
            logError [qmb|
              {CrmEg02} is failed on parsing response data \
              from Era Glonass to check status \
              of CaRMa "ephemeral VINs" \
              ("{eraGlonassSynchronizedContractTypeRep}"s).
              \  Error message: {errorMessage}
              \  Incorrect response body: {incorrectResponseBody}
            |]

            throwM $
              EGCheckVinResponseIsFailed errorMessage incorrectResponseBody

          responseResolver EGCheckVinResponse {..} = do
            ourCode <- asks carmaEgServiceCode

            -- Checking for data correctness
            uncurry when $
              let
                lenRequests  = length requestsList
                lenResponses = length responses

                failMsg = [qmb|
                  Incorrect response to a request to check status of \
                  CaRMa "ephemeral VINs" \
                  ("{eraGlonassSynchronizedContractTypeRep}"s).
                  Unexpectedly count of responses (checked VINs) \
                  is not equal to count of requests (VINs to check).
                  \  Count of responses (checked VINs) is {lenResponses}.
                  \  Count of requests (VINs to check) is {lenRequests}.
                |]

                m = do
                  logError [qm| {CrmEg02} is failed due to: {failMsg} |]
                  fail failMsg
              in
                (lenRequests /= lenResponses, m)

            let vinsAreNotEqualFailMsg contractId reqVin resVin = [qmb|
                  Incorrect response to a request to check status of \
                  CaRMa "ephemeral VINs" \
                  ("{eraGlonassSynchronizedContractTypeRep}"s).
                  Unexpectedly a VIN from response is not equal to one
                  from request (at the same position in order).
                  \  {contractTypeRep} id is {fromSqlKey contractId}.
                  \  VIN from request is {reqVin}.
                  \  VIN from response is {resVin}.
                |]

            let -- | Reducer which extracts
                reduceFn
                  :: Vec.Vector EraGlonassSynchronizedContractId
                  -> ( EraGlonassSynchronizedContractId
                     , EGCheckVinRequestRequests
                     , EGCheckVinResponseResponses
                     )
                  -> Either String
                            (Vec.Vector EraGlonassSynchronizedContractId)

                reduceFn acc ( contractId
                             , EGCheckVinRequestRequests { vin = reqVin }
                             , EGCheckVinResponseResponsesVinExists
                                 { vin = resVin, vinProviders }
                             ) = do

                  -- Checking for data correctness
                  when (reqVin /= resVin) $
                    Left $ vinsAreNotEqualFailMsg contractId reqVin resVin

                  Right $ if any f vinProviders
                             then Vec.snoc acc contractId
                             else acc -- Already /unmarked/
                  where
                    -- | A predicate which checks that VIN is handled by us
                    f EGCheckVinResponseVinProviders {..} = code == ourCode

                reduceFn acc ( contractId
                             , EGCheckVinRequestRequests { vin = reqVin }
                             , EGCheckVinResponseResponsesVinNotExists
                                 { vin = resVin }
                             ) =
                  if reqVin == resVin -- Checking for data correctness
                     then Right acc -- Already /unmarked/
                     else Left
                        $ vinsAreNotEqualFailMsg contractId reqVin resVin

            let result
                  = fmap Vec.toList
                  $ foldM reduceFn Vec.empty
                  $ zip3 (entityKey <$> entities) requestsList responses

            case result of
                 Right x  -> pure x
                 Left msg -> do
                   logError [qm| {CrmEg02} is failed due to: {msg} |]
                   fail msg

      (asks egClientEnv >>= runClientM (crmEG02Post egRequestData))
        >>= requestResolver
        >>= responseResolver


contractTypeRep, eraGlonassSynchronizedContractTypeRep :: TypeRep
contractTypeRep = typeRep (Proxy :: Proxy Contract)
eraGlonassSynchronizedContractTypeRep =
  typeRep (Proxy :: Proxy EraGlonassSynchronizedContract)
