{-# LANGUAGE OverloadedStrings, QuasiQuotes, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, TypeApplications, ViewPatterns, DataKinds #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards, BangPatterns, LambdaCase #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Request for service status synchronizer worker module.
module Carma.EraGlonass.StatusSynchronizer
     ( runStatusSynchronizer
     ) where

import           Data.Proxy
import           Data.Semigroup (Semigroup ((<>)))
import           Data.Function (fix)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Text.InterpolatedString.QM
import           Text.Printf (printf)

import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Reader (MonadReader, ReaderT, asks)
import           Control.Monad.Catch (MonadCatch, MonadThrow (throwM))
import           Control.Arrow ((&&&), (***), (>>>))
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Control.Exception (SomeException, fromException)

import           Database.Persist ((==.), (=.))
import           Database.Persist.Sql (SqlBackend, fromSqlKey)
import           Database.Persist.Types (SelectOpt (Asc), Entity (..))

import           Carma.Model.LegacyTypes (Phone (Phone))
import           Carma.Monad
import           Carma.Utils
import           Carma.Utils.StringyEnum
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Instance.Persistent (TimeoutException (..))
import           Carma.EraGlonass.Client
import           Carma.EraGlonass.Helpers
import           Carma.EraGlonass.Types.AppContext (AppContext (..))
import           Carma.EraGlonass.Types.Helpers.DateTime (showRFC3339DateTime)
import           Carma.EraGlonass.Types.Helpers.NonEmptyText
import           Carma.EraGlonass.Types.EGDateTime
import           Carma.EraGlonass.Types.EGChangeRequestStatusRequest
import           Carma.EraGlonass.Types.EGChangeProcessingStatusRequest
import           Carma.EraGlonass.Types.EGMayFailToParse
import           Carma.EraGlonass.StatusSynchronizer.Helpers
import           Carma.EraGlonass.StatusSynchronizer.Types
import           Carma.EraGlonass.Model.CaseEraGlonassCreateRequest.Persistent
import           Carma.Model.EraGlonassCaseStatusUpdate.Persistent

import           Carma.EraGlonass.Types.EGIntegrationPoint
                   ( EGIntegrationPoint
                       ( ChangeProcessingStatus
                       , ChangeRequestStatus
                       )
                   )


runStatusSynchronizer
  :: forall m
   .
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadDelay m -- To wait before next synchronization.
   , MonadThread m
   , MonadSTM m
   , MonadPersistentSql m
   , MonadCatch m
   , MonadClock m
   , MonadServantClient m
   )
  => m ()

runStatusSynchronizer = do
  srcLogInfo "Running Status Synchronizer worker..."

  let syncResolve :: Either SomeException () -> m ()
      syncResolve (Right ()) =
        srcLogInfo [qn| Statuses synchronization is finished successfully. |]

      syncResolve (Left (fromException -> Just (TimeoutExceeded n))) =
        saveFailureIncidentInBackground
          (Proxy @'ChangeRequestStatus)
          FailureWithoutBody
          [qms|
            Synchronization of statuses is failed becuase it is exceeded timeout
            of {round $ (fromIntegral n / 1000 / 1000 :: Float) :: Int}
            second(s).
          |]

      syncResolve
        -- Only response is possible to fail to parse since we don't have
        -- incoming requests in this integration point.
        (Left (fromException -> Just (ResponseParseFailure err body))) =
          saveFailureIncidentInBackground
            (Proxy @'ChangeRequestStatus)
            (FailureWithBody @'FailureResponseBodyType body)
            [qms|
              Synchronization of statuses is failed becuase response from EG
              service is failed to parse with this error message: {err}
            |]

      syncResolve (Left (fromException -> Just (FailureScenario err))) =
        saveFailureIncidentInBackground
          (Proxy @'ChangeRequestStatus)
          FailureWithoutBody
          [qms|
            Synchronization of statuses is failed becuase EG service
            responded with failure case: {err}
          |]

      syncResolve (Left exception) =
        saveFailureIncidentInBackground
          (Proxy @'ChangeRequestStatus)
          FailureWithoutBody
          [qms|
            Synchronization of statuses is failed with exception: {exception}.
          |]

  forever $ do
    manualTriggerBus <- asks statusSynchronizerTriggerBus
    bgThreadsCounter <- asks backgroundTasksCounter
    interval         <- asks statusSynchronizerInterval

    (wait, done) <- atomically $ do
      -- Flusing previous state
      possibly manualTriggerBus $ fmap (const ()) . tryTakeTMVar

      modifyTVar' bgThreadsCounter (+2) -- Two threads in background

      (newEmptyTMVar <&>) $
        takeTMVar  &&& flip putTMVar () >>>
        atomically *** atomically

    let -- | Deducing background threads counter when thread is done
        onForkDeath :: m ()
        onForkDeath = atomically $ modifyTVar' bgThreadsCounter pred

    intervalThread <-
      flip forkFinally (const onForkDeath) $ do
        delay interval

        let inMinutes :: Int -> Float
            inMinutes microseconds =
              fromIntegral microseconds / 1000 / 1000 / 60

        srcLogDebug [qms|
          Interval of {printf "%.2f" $ inMinutes interval :: String} minute(s)
          is passed, initiating statuses synchronization...
        |]

        done

    manualTriggerThread <-
      flip forkFinally (const onForkDeath) $
        possibly manualTriggerBus $ \bus -> do
          time <- atomically $ readTMVar bus
          srcLogDebug [qms|
            Received signal from manual statuses synchronization trigger bus,
            it's triggered at {showRFC3339DateTime time :: Text},
            running statuses synchronization process manually...
          |]
          done

    wait

    srcLogDebug [qns|
      Killing waiting threads before running statuses synchronization...
    |]

    killThread intervalThread
    killThread manualTriggerThread

    asks statusSynchronizerTimeout
      >>= flip runSqlTimeout synchronizeStatuses
      >>= syncResolve


-- | Log message about that synchronization is finished successfully
--   mentioning how many interations it have taken.
doneSyncLog :: MonadLoggerBus m => Word -> m ()
doneSyncLog iterations = srcLogInfo [qms|
  Synchronizing statuses is successfully done,
  it took {iterations} iteration(s).
|]


synchronizeStatuses
  :: forall m
   .
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadPersistentSql m
   , MonadServantClient m
   , MonadThrow m
   , MonadClock m
   )
  => ReaderT SqlBackend m ()

synchronizeStatuses =
  -- @(1 &)@ means it's first iteration.
  (srcLogInfo [qn| Synchronizing statuses... |] >>) $ (1 &) $ fix $ \((

    let
      preLog (i :: Word) = srcLogDebug [qms|
        Committing statuses synchronization transaction (interation: {i})
        before running it again...
      |]

      postLog (i :: Word) = srcLogDebug [qms|
        Running statuses synchronization again (interation: {i})...
      |]
    in
      \m i -> preLog i >> transactionSave >> postLog (succ i) >> m (succ i)

  -- @repeat'@ is an \"again" function produced by "Data.Function.fix",
  -- but modified above by adding log messaging, commiting transaction.
  --
  -- @repeatWholeOperation@ is like \"again" with pre-applied iteration counter,
  -- so you don't have to care about passing the argument and about correctness
  -- of the value.
  --
  -- @doneLog@ is just pre-applied "doneSyncLog" with iteration counter,
  -- you either call @repeatWholeOperation@ or @doneLog@ at the end.
  ) -> repeat') -> \( (repeat' &&& lift . doneSyncLog) ->
                      (repeatWholeOperation, doneLog)
                    ) -> do

  statusUpdate <-
    selectFirst
      [ EraGlonassCaseStatusUpdateIsProcessed ==. False
      ]
      [ Asc EraGlonassCaseStatusUpdateCtime
      ]

  case statusUpdate of
       Nothing -> doneLog
       Just Entity { entityKey = statusUpdateId,
                     entityVal = EraGlonassCaseStatusUpdate {..} } -> do

         !requestId' <-
           selectFirst
             [ CaseEraGlonassCreateRequestAssociatedCase ==.
                 eraGlonassCaseStatusUpdateCaseId
             ] mempty
             >>= \case Just Entity { entityVal =
                                       CaseEraGlonassCreateRequest {..} } ->
                         pure caseEraGlonassCreateRequestRequestId

                       Nothing -> error [qms|
                         Failed to obtain EGRequestId for status update
                         #{fromSqlKey statusUpdateId}!
                       |]

         do
           response <-
             let
               req
                 = EGChangeRequestStatusRequest
                 { requestId = requestId'

                 , statusCode =
                     case eraGlonassCaseStatusUpdateNewCaseStatus of
                          x | x == toStringy WorkInProgress -> WorkInProgress
                            | x == toStringy Done           -> Done
                            | x == toStringy ClientDenial   -> ClientDenial
                          x -> error [qms|
                            Unexpected "newCaseStatus" value for status update
                            #{fromSqlKey statusUpdateId}: "{x}"
                          |]

                 , statusTime =
                     EGDateTime $ fromMaybe eraGlonassCaseStatusUpdateCtime
                                            eraGlonassCaseStatusUpdateMtime

                 , comment = Nothing
                 }
             in
               lift (asks egClientEnv)
                 >>= lift . runClientM (changeRequestStatusRequest [req])
                 >>= either throwM pure
                 >>= \case SuccessfullyParsed x -> pure x
                           FailedToParse err body ->
                             throwM $ ResponseParseFailure [qms|
                               Failed to parse the response of an attempt to
                               synchronize case statuses via
                               {ChangeRequestStatus} (current status update is
                               #{fromSqlKey statusUpdateId}) with this error
                               message: {err}
                             |] body

           case response of
                EGChangeProcessingStatusResponseOk Nothing -> pure ()
                EGChangeProcessingStatusResponseOk (Just []) -> pure ()
                EGChangeProcessingStatusResponseOk (Just [x]) ->
                  srcLogWarn [qms|
                    Got an error in response for status update
                    #{fromSqlKey statusUpdateId}: {x}
                  |]
                EGChangeProcessingStatusResponseOk (Just xs) -> error [qms|
                  Unexpected result for {ChangeRequestStatus} errors response
                  for status update #{fromSqlKey statusUpdateId}, errors list:
                  {xs}
                |]

                EGChangeProcessingStatusResponseFailure { resultCode,
                                                          description } -> do

                  let descriptionText =
                        maybe " was not provided"
                              ((": " <>) . fromNonEmptyText)
                              description

                  let errorMsg = [qms|
                        Failed to synchronize status update, result code is
                        {resultCode}, textual failure
                        description{descriptionText}
                      |]

                  srcLogError [qm| {errorMsg} |]
                  throwM $ FailureScenario errorMsg

         when ( let x = eraGlonassCaseStatusUpdateNewCaseStatus
                 in x == toStringy Done || x == toStringy ClientDenial ) $ do

           contractId' <- lift $ asks statusSynchronizerContractId

           response' <-
             let
               req
                 = EGChangeProcessingStatusRequest
                 { requestId = requestId'

                 , ivsPhoneNumber =
                     eraGlonassCaseStatusUpdateTerminalPhone >>= \case
                       Phone x -> toNonEmptyText x

                 , fullName =
                     eraGlonassCaseStatusUpdateCustomerName >>= toNonEmptyText

                 , phoneNumber =
                     eraGlonassCaseStatusUpdateCustomerPhone >>= \case
                       Phone x -> toNonEmptyText x

                 , statusCode = Closed
                 , serviceCategoryId = Nothing
                 , serviceName = Nothing
                 , contractId = contractId'
                 }
             in
               lift (asks egClientEnv)
                 >>= lift . runClientM (changeProcessingStatusRequest [req])
                 >>= either throwM pure
                 >>= \case SuccessfullyParsed x -> pure x
                           FailedToParse err body ->
                             throwM $ ResponseParseFailure [qms|
                               Failed to parse the response of an attempt to
                               synchronize case statuses via
                               {ChangeProcessingStatus} (current status update
                               is #{fromSqlKey statusUpdateId}) with this error
                               message: {err}
                             |] body

           -- We don't really care about such errors
           case response' of
                EGChangeProcessingStatusResponseOk Nothing -> pure ()
                EGChangeProcessingStatusResponseOk (Just []) -> pure ()
                EGChangeProcessingStatusResponseOk (Just [x]) ->
                  srcLogWarn [qms|
                    Got an error in response for status update
                    #{fromSqlKey statusUpdateId}: {x}
                  |]
                EGChangeProcessingStatusResponseOk (Just xs) -> error [qms|
                  Unexpected result for {ChangeProcessingStatus} errors response
                  for status update #{fromSqlKey statusUpdateId}, errors list:
                  {xs}
                |]

                EGChangeProcessingStatusResponseFailure { resultCode,
                                                          description } -> do

                  let descriptionText =
                        maybe " was not provided"
                              ((": " <>) . fromNonEmptyText)
                              description

                  let errorMsg = [qms|
                        Failed to synchronize status update, result code is
                        {resultCode}, textual failure
                        description{descriptionText}
                      |]

                  srcLogError [qm| {errorMsg} |]
                  throwM $ FailureScenario errorMsg

         now <- lift getCurrentTime

         update statusUpdateId
           [ EraGlonassCaseStatusUpdateIsProcessed =. True
           , EraGlonassCaseStatusUpdateProcessTime =. Just now
           ]

         repeatWholeOperation
