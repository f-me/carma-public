{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf, LambdaCase #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}

module Carma.EraGlonass.VinSynchronizer.UnmarkAsHandled
     ( unmarkAsHandled
     ) where

import           Data.Proxy
import           Data.Monoid
import           Data.Typeable
import           Data.Text (Text, unpack)
import           Text.InterpolatedString.QM
import qualified Data.Vector as Vec
import           Data.List.NonEmpty (toList)

import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Reader (ReaderT, asks)
import           Control.Monad.Catch

import           Database.Persist.Sql (ToBackendKey, SqlBackend, fromSqlKey)
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
import           Carma.EraGlonass.VinSynchronizer.Helpers
import           Carma.EraGlonass.Helpers


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

  let totalCount = length contracts + length ephemerals
  let isEverythingAlreadyUnmarked = totalCount == 0

  logDebug [qmb|
    {CrmEg02}: Done with checking VINs for which of them supposed to be \
    "unmarked" as handled by us.
    Counts of VINs which are supposed to be "unmarked":
    \  {contractsLogInfix}: {length contracts}
    \  {ephemeralsLogInfix}: {length ephemerals}
    { if isEverythingAlreadyUnmarked
         then "Every of those VINs is already \"unmarked\"."
         else "Building request to \"unmark\" those VINs..." :: Text
    }
  |]

  unless isEverythingAlreadyUnmarked $
    lift $ askEGToUnmarkAsHandled contracts ephemerals

  -- TODO deactivate those VINs on our side


askEGToUnmarkAsHandled
  :: VinSynchronizerMonad m
  => [(ContractId, EGVin)]
  -> [(EraGlonassSynchronizedContractId, EGVin)]
  -> m ()

askEGToUnmarkAsHandled contracts ephemerals = do
  reqId <- newRequestId

  let egRequestData =
        EGDeleteVinRequest
          { requestId = reqId
          , requests  = requests'
          }

  let -- | Resolver for possible failure of HTTP-request.
      requestResolver (Right x) = pure x
      requestResolver (Left  e) = do
        logError [qmb|
          {CrmEg02} is failed on DELETE request to Era Glonass to "unmark" \
          {targetLog}.
          \  {contractsLogInfix}: {contracts}
          \  {ephemeralsLogInfix}: {ephemerals}
          Error: {e}
        |]

        throwM $ EGDeleteVinRequestIsFailed e egRequestData

  let responseResolver EGDeleteVinResponseIncorrect {..} = do
        logError [qmb|
          {CrmEg02} is failed on parsing response data from Era Glonass \
          to "unmark" {targetLog}.
          \  Error message: {errorMessage}
          \  Incorrect response body: {incorrectResponseBody}
        |]

        throwM $
          EGDeleteVinResponseIsFailed errorMessage incorrectResponseBody

      responseResolver EGDeleteVinResponse {..} = do
        let -- | Checking for data correctness
            checkCounts logInfix (a, aLog) (b, bLog) =
              checkRequestsAndResponsesCountsEquality
                CrmEg02 (actionLog logInfix)
                (length a, t aLog)
                (length b, t bLog)

        checkCounts targetLog
          (requests', [qn| checked VINs |])
          (responses, [qn| checked VINs |])

        let (contractsResponses, ephemeralsResponses) =
              splitAt (length contracts) responses

        checkCounts contractsLogInfix
          (contracts,          [qn| VINs to "unmark" |])
          (contractsResponses, [qn| "unmarked" VINs |])

        checkCounts ephemeralsLogInfix
          (ephemerals,          [qn| VINs to "unmark" |])
          (ephemeralsResponses, [qn| "unmarked" VINs |])

        let checks
              :: (VinSynchronizerMonad m, Functor f)
              => UnmarkingVinType
              -> f (EGVin, EGDeleteVinResponseResponses)
              -> f (m ())

            checks vinType = fmap $ \case

              (reqVin, EGDeleteVinResponseResponsesSuccess { vin = resVin }) ->
                case checkForRequestVinAndResponseVinEquality
                       log' ([] :: [Text]) reqVin resVin
                  of Right x -> pure x
                     Left msg -> do
                       logError [qm| {CrmEg02} is failed due to: {msg} |]
                       fail msg

              (reqVin, EGDeleteVinResponseResponsesIncorrectFormat {..}) -> do
                logError [qms|
                  {CrmEg02} is failed on checking response data from
                  Era Glonass {log'}. Era Glonass service responded
                  that {reqVin} has incorrect format.
                |]

                throwM
                  $ EGDeleteVinIncorrectFormat vinType reqVin
                  $ Data.Text.unpack <$> statusDescription

              where
                log' = case vinType of
                  UnmarkingContractVinType  -> actionLog contractsLogInfix
                  UnmarkingEphemeralVinType -> actionLog ephemeralsLogInfix

        sequence_ $ checks UnmarkingContractVinType
                  $ fmap snd contracts `zip` contractsResponses

        sequence_ $ checks UnmarkingEphemeralVinType
                  $ fmap snd ephemerals `zip` ephemeralsResponses

  (asks egClientEnv >>= runClientM (crmEG02Delete egRequestData))
    >>= requestResolver
    >>= responseResolver

  where
    actionLog :: Text -> Text
    actionLog logInfix = [qm| to "unmark" {logInfix} |]

    requests'
      =  fmap (\(_, x) -> EGDeleteVinRequestRequests { vin = x }) contracts
      <> fmap (\(_, x) -> EGDeleteVinRequestRequests { vin = x }) ephemerals

    targetLog
      = if
      | null contracts  -> contractsLogInfix
      | null ephemerals -> ephemeralsLogInfix
      | otherwise ->
          [qm| both {contractsLogInfix} and {ephemeralsLogInfix} |]


-- | Returns a list of "Contract"s which are handled by us, so these
--   we're supposed to /unmark/ (filtered only those who match our own
--   provider).
checkContracts
  :: forall m model
  .  (VinSynchronizerMonad m, model ~ Contract)
  => [Entity model]
  -> m [(Key model, EGVin)]

checkContracts = go where
  go = checkVINs logSuffix vinGetter
  modelTypeRep = typeRep (Proxy :: Proxy model)
  logSuffix = [qm| {contractsLogInfix} |]

  vinGetter Entity {..} =
    case contractVin entityVal <&> textToProofedEGVin of
         Just x@(Right _) -> x

         Just (Left msg) -> Left [qms|
           VIN of a "{modelTypeRep}" with id {fromSqlKey entityKey} is
           incorrect. Error message: {msg}
         |]

         Nothing -> Left [qms|
           VIN of a "{modelTypeRep}" with id {fromSqlKey entityKey} could not be
           "Nothing", it supposed to be checked earlier.
         |]


-- | Returns a list of "EraGlonassSynchronizedContract"s
--   which are handled by us, so these we're supposed to /unmark/
--   (filtered only those who match our own provider).
checkEphemeral
  :: forall m model
  .  (VinSynchronizerMonad m, model ~ EraGlonassSynchronizedContract)
  => [Entity model]
  -> m [(Key model, EGVin)]

checkEphemeral = go where
  go = checkVINs logSuffix vinGetter
  modelTypeRep = typeRep (Proxy :: Proxy model)
  logSuffix = [qm| {ephemeralsLogInfix} |]

  vinGetter Entity {..} =
    case eraGlonassSynchronizedContractVin entityVal & textToProofedEGVin of
         x@(Right _) -> x
         Left msg -> Left [qms|
           VIN of a "{modelTypeRep}" with id {fromSqlKey entityKey} is incorrect
           (which is really strange, it supposed to be validated earlier, when
           VIN have been synchronized, retrieved as proofed "EGVin" and stored
           to the database while being valid). Error message: {msg}
         |]


-- | Generalized monad to check VINs whether they need to be /unmarked/ as
-- handled by us.
checkVINs
  :: forall m model
  .  ( VinSynchronizerMonad m
     , ToBackendKey SqlBackend model
     , Typeable model
     )
  => Text -- ^ Log infix of what kind of VINs we're checking
  -> (Entity model -> Either String EGVin)
  -- ^ Helper to obtain validated VIN from a model
  -> [Entity model]
  -> m [(Key model, EGVin)]
  -- ^ Returns a list of Model items linked with VINs which are handled by us,
  --   so these we're supposed to /unmark/
  --   (filtered only those who match our own service provider).

checkVINs modelLogInfix getVinFromModel entities = do
  logDebug [qms|
    Checking status of CaRMa {modelLogInfix}
    on Era Glonass side (checking whether they're handled by us
    so we have to "unmark" them as handled by us)...
  |]

  let -- | @Left@ if some VIN is incorrect.
      requests :: Either String [EGCheckVinRequestRequests]
      requests = Vec.toList <$> foldM reduceFn Vec.empty entities where
        reduceFn acc entity =
          getVinFromModel entity <&> \proofedVin ->
          Vec.snoc acc EGCheckVinRequestRequests { vin = proofedVin }

  requestsList <- case requests of
    Right x -> pure x
    Left  e ->
      logError [qms|
        {CrmEg02} is failed on constructing request to check status of
        CaRMa {modelLogInfix}. Error: {e}
      |] >> fail e

  -- Checking for data correctness
  checkConstructredRequestsAndEntitiesCountsEquality
    CrmEg02 actionLog
    (length requestsList, t [qn| VINs to check |])
    (length entities,     typeRep (Proxy :: Proxy model))

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
          status of CaRMa {modelLogInfix}. Error: {e}
        |]

        throwM $ EGCheckVinRequestIsFailed e egRequestData

  let responseResolver EGCheckVinResponseIncorrect {..} = do
        logError [qmb|
          {CrmEg02} is failed on parsing response data from Era Glonass \
          {actionLog}.
          \  Error message: {errorMessage}
          \  Incorrect response body: {incorrectResponseBody}
        |]

        throwM $
          EGCheckVinResponseIsFailed errorMessage incorrectResponseBody

      responseResolver EGCheckVinResponse {..} = do
        ourCode <- asks carmaEgServiceCode

        -- Checking for data correctness
        checkRequestsAndResponsesCountsEquality
          CrmEg02 actionLog
          (length requestsList, t [qn| VINs to check |])
          (length responses,    t [qn| checked VINs |])

        let reduceFn
              :: Vec.Vector (Key model, EGVin)
              -> ( Key model
                 , EGCheckVinRequestRequests
                 , EGCheckVinResponseResponses
                 )
              -> Either String (Vec.Vector (Key model, EGVin))

            reduceFn acc ( modelId
                         , EGCheckVinRequestRequests { vin = reqVin }
                         , EGCheckVinResponseResponsesVinExists
                             { vin = resVin, vinProviders }
                         ) = do

              -- Checking for data correctness
              checkForRequestVinAndResponseVinEquality
                actionLog [modelLog modelId] reqVin resVin

              Right $ if any f vinProviders
                         then Vec.snoc acc (modelId, resVin)
                         else acc -- Already /unmarked/
              where
                -- | A predicate which checks that VIN is handled by us
                f EGCheckVinResponseVinProviders {..} = code == ourCode

            reduceFn acc ( modelId
                         , EGCheckVinRequestRequests { vin = reqVin }
                         , EGCheckVinResponseResponsesVinNotExists
                             { vin = resVin }
                         ) = do

              -- Checking for data correctness
              checkForRequestVinAndResponseVinEquality
                actionLog [modelLog modelId] reqVin resVin

              Right acc -- Already /unmarked/

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

  where
    actionLog = t [qm| to check status of CaRMa {modelLogInfix} |]

    modelLog modelId =
      t [qm| {typeRep (Proxy :: Proxy model)} id is {fromSqlKey modelId} |]


contractsLogInfix :: Text
contractsLogInfix =
  [qm| outdated/deactivated "{typeRep (Proxy :: Proxy Contract)}"s |]

ephemeralsLogInfix :: Text
ephemeralsLogInfix =
  [qms| "ephemeral VINs"
        ("{typeRep (Proxy :: Proxy EraGlonassSynchronizedContract)}"s) |]

-- | Just a shorthand for type infering
t :: Text -> Text
t = id
