{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, LambdaCase #-}
{-# LANGUAGE QuasiQuotes, ViewPatterns, NamedFieldPuns, TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, TypeFamilies, DataKinds #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeOperators, RecordWildCards #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | VIN synchronizer worker module.
module Carma.EraGlonass.VinSynchronizer
     ( runVinSynchronizer
     ) where

import           Prelude hiding (fail)
import           GHC.Generics
import           GHC.TypeLits

import           Data.Proxy
import           Data.Semigroup ((<>))
import           Data.Typeable (Typeable)
import           Data.Function (fix)
import           Data.Time.Clock (utctDay)
import           Data.Time.LocalTime (TimeZone)
import           Data.Text (Text)
import           Text.InterpolatedString.QM
import           Text.Printf (printf)
import           Data.List.NonEmpty (NonEmpty ((:|)))

import           Control.Arrow ((&&&), (***), (>>>))
import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Reader (MonadReader, ReaderT, asks)
import           Control.Monad.Catch (MonadCatch)
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Control.Exception (SomeException, fromException)

import           Database.Persist.Class (PersistEntity, EntityField)
import           Database.Persist.Sql (SqlBackend)
import           Database.Esqueleto (Single (..))

import           Carma.Monad
import           Carma.Model.Contract.Persistent
import           Carma.Model.Program.Persistent
import           Carma.Model.SubProgram.Persistent
import           Carma.Utils.Operators
import           Carma.Utils.TypeSafe.Generic.DataType
import           Carma.Utils.TypeSafe.TypeFamilies
import           Carma.Utils.Persistent.RawSqlQueryConstructor
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Instance.Persistent (TimeoutException (..))
import           Carma.EraGlonass.Helpers
import           Carma.EraGlonass.Model.EraGlonassSynchronizedContract.Persistent
import           Carma.EraGlonass.VinSynchronizer.Types
import           Carma.EraGlonass.VinSynchronizer.Helpers
import           Carma.EraGlonass.VinSynchronizer.UnmarkAsHandled
import           Carma.EraGlonass.VinSynchronizer.SynchronizeContracts
import           Carma.EraGlonass.Types.AppContext (AppContext (..))
import           Carma.EraGlonass.Types.EGVin (EGVin, egVinPosixRegex)
import           Carma.EraGlonass.Types.Helpers.DateTime (showRFC3339DateTime)


-- | VIN synchronizer worker starter.
runVinSynchronizer
  :: forall m
   .
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadClock m
   , MonadDelay m -- To wait before synchronizations.
   , MonadPersistentSql m
   , MonadRawEsqueleto m
   , MonadServantClient m
   , MonadThread m
   , MonadCatch m
   , MonadSTM m
   )
  => TimeZone
  -> m ()

runVinSynchronizer tz = go where
  -- | Converts microseconds to hours with fractional part.
  inHours :: Int -> Float
  inHours microseconds = fromIntegral microseconds / 1000 / 1000 / 3600

  willBeRetried interval = [qms|
    Synchronization of VINs will be retried in
    {printf "%.2f" (inHours interval) :: String} hour(s)
    (also it will wait for manual trigger in parallel, flushing manual
    triggering bus first, either scheduled retry or manual trigger will run next
    synchronization attempt)...
  |] :: Text

  retrying interval = [qms|
    Retrying to synchronize VINs after an interval of
    {printf "%.2f" (inHours interval) :: String} hour(s)...
  |] :: Text

  go = do
    srcLogInfo [qm| Running VIN synchronizer worker (timezone is {tz})... |]

    forever $ do
      (hoursToWait, minutesToWait) <- getTimeToWait tz

      srcLogDebug $ let zeroPad x = printf "%02d" (x :: Word) :: String in [qms|
        Waiting for {zeroPad $ floor hoursToWait}:{zeroPad minutesToWait}
        before 00:00 to trigger next VIN synchronization (also waiting for
        manual trigger in paralell, flushing manual triggering bus first,
        either scheduled iteration or manual trigger will run next
        synchronization)...
      |]

      manualTriggerBus <- asks vinSynchronizerTriggerBus
      bgThreadsCounter <- asks backgroundTasksCounter

      (wait, done) <- atomically $ do
        _ <- tryTakeTMVar manualTriggerBus -- Flusing previous state
        modifyTVar' bgThreadsCounter (+2) -- Two threads in background

        (newEmptyTMVar <&>) $
          takeTMVar  &&& flip putTMVar () >>>
          atomically *** atomically

      let -- | Deducing background threads counter when thread is done
          onForkDeath = atomically $ modifyTVar' bgThreadsCounter pred

      scheduleThread <-
        flip forkFinally (const onForkDeath) $ do
          delay $ round $ hoursToWait * 3600 * (10 ** 6)
          srcLogDebug [qns|
            It's about 00:00, initiating scheduled
            VIN synchronization process...
          |]
          done

      manualTriggerThread <-
        flip forkFinally (const onForkDeath) $ do
          time <- atomically $ readTMVar manualTriggerBus
          srcLogDebug [qms|
            Received signal from manual VIN synchronization trigger bus,
            it's triggered at {showRFC3339DateTime time :: Text},
            running VIN synchronization process manually...
          |]
          done

      wait
      srcLogInfo "Killing waiting threads before running VIN synchronization..."
      killThread scheduleThread
      killThread manualTriggerThread

      fix $ \again ->
        asks vinSynchronizerTimeout
          >>= flip runSqlTimeout synchronizeVins
          >>= synchronizationResolve
          >>= flip unless again

  -- | Returned @Bool@ indicates successfulness status.
  --
  -- @True@ means it is done, @False@ means it is failed.
  synchronizationResolve
    ::
     ( MonadReader AppContext m
     , MonadLoggerBus m
     , MonadPersistentSql m
     , MonadThread m
     , MonadClock m
     , MonadDelay m -- To wait before retry
     , MonadCatch m -- To catch failures in background
     , MonadSTM m
     )
    => Either SomeException ()
    -> m Bool

  synchronizationResolve (Right ()) = True <$
    srcLogInfo [qn| VINs synchronization iteration is finished successfully. |]

  synchronizationResolve (Left (fromException -> Just (TimeoutExceeded n))) =
    False <$ retryFlow FailureWithoutBody [qms|
      Synchronization of VINs is failed becuase it is exceeded timeout of
      {round $ (fromIntegral n / 1000 / 1000 :: Float) :: Int} second(s).
    |]

  synchronizationResolve
    -- Only response is possible to fail to parse since we don't have incoming
    -- requests in this integration point.
    (Left (fromException -> Just (ResponseParseFailure err body))) =
      False <$ retryFlow (FailureWithBody @'FailureResponseBodyType body) [qms|
        Synchronization of VINs is failed becuase response from EG service
        is failed to parse with this error message: {err}
      |]

  synchronizationResolve (Left (fromException -> Just (FailureScenario err))) =
    False <$ retryFlow FailureWithoutBody [qms|
      Synchronization of VINs is failed becuase EG service
      responded with failure case: {err}
    |]

  synchronizationResolve (Left exception) =
    False <$ retryFlow FailureWithoutBody [qms|
      Synchronization of VINs is failed with exception: {exception}.
    |]

  retryFlow
    ::
     ( MonadReader AppContext m
     , MonadLoggerBus m
     , MonadThread m
     , MonadClock m
     , MonadDelay m -- To wait before retry
     , MonadSTM m
     , OneOf bodyType '[ 'FailureNoBodyType, 'FailureResponseBodyType ]
     , GetFailureBodyType bodyType
     )
    => FailureBody bodyType
    -> Text
    -> m ()

  retryFlow failureBody failureMessage = do
    retryInterval <- asks vinSynchronizerRetryInterval
    srcLogError [qm| {failureMessage} {willBeRetried retryInterval} |]
    saveFailureIncidentInBackground failureBody failureMessage
    manualTriggerBus <- asks vinSynchronizerTriggerBus
    bgThreadsCounter <- asks backgroundTasksCounter

    (wait, done) <- atomically $ do
      _ <- tryTakeTMVar manualTriggerBus -- Flusing previous state
      modifyTVar' bgThreadsCounter (+2) -- Two threads in background

      (newEmptyTMVar <&>) $
        takeTMVar  &&& flip putTMVar () >>>
        atomically *** atomically

    let -- | Deducing background threads counter when thread is done
        onForkDeath = atomically $ modifyTVar' bgThreadsCounter pred

    scheduleThread <-
      flip forkFinally (const onForkDeath) $ do
        delay retryInterval
        srcLogInfo $ retrying retryInterval
        done

    manualTriggerThread <-
      flip forkFinally (const onForkDeath) $ do
        time <- atomically $ readTMVar manualTriggerBus
        srcLogDebug [qms|
          Received signal from manual VIN synchronization trigger bus,
          it's triggered at {showRFC3339DateTime time :: Text},
          retrying VIN synchronization manually...
        |]
        done

    wait
    srcLogInfo "Killing waiting threads before retrying VIN synchronization..."
    killThread scheduleThread
    killThread manualTriggerThread


-- | Log message about that VINs synchronization is finished successfully
--   mentioning how many interations it have taken.
doneSyncVinsLog :: MonadLoggerBus m => Word -> m ()
doneSyncVinsLog iterations = srcLogInfo [qms|
  Synchronizing VINs is successfully done, it took {iterations} iteration(s).
|]


-- | VINs synchronization logic handler.
--
-- If you don't see comments for code just look at log messages nearby,
-- it may tell you a lot instead.
synchronizeVins
  :: forall m
   .
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadPersistentSql m
   , MonadRawEsqueleto m
   , MonadServantClient m
   , MonadClock m
   , MonadCatch m
   , MonadThread m
   , MonadSTM m
   )
  => ReaderT SqlBackend m ()

synchronizeVins =
  -- @(1 &)@ means it's first iteration.
  (srcLogInfo [qn| Synchronizing VINs... |] >>) $ (1 &) $ fix $ \((

    let
      preLog (i :: Word) = srcLogDebug [qms|
        Committing VIN synchronization transaction (interation: {i})
        before running whole operation again...
      |]

      postLog (i :: Word) = srcLogDebug [qms|
        Running VIN synchronization again (interation: {i})...
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
  -- @doneLog@ is just pre-applied "doneSyncVinsLog" with iteration counter,
  -- you either call @repeatWholeOperation@ or @doneLog@ at the end.
  ) -> repeat') -> \( (repeat' &&& lift . doneSyncVinsLog) ->
                      (repeatWholeOperation, doneLog)
                    ) -> do

  nowDay <- utctDay <$> lift getCurrentTime
  batchSize <- lift $ asks vinSynchronizerBatchSize

  srcLogDebug $
    let
      contract = typeName (Proxy :: Proxy Contract) :: Text
    in [qms|
      Getting a list of "{contract}"s (VINs actually) which are used to be
      handled by us (now we need to notify EG service that they
      aren't handled by CaRMa anymore)
      and incorrect data ("ephemeral VINs"), when some VIN is
      handled by CaRMa but no longer represented in "{contract}"s list
      (usually it doesn't happen but if for instance you change "vin" field
      value of a "{contract}" and previously "vin" of that "{contract}" have
      been marked as handled by CaRMa for EG service we supposed to notify EG
      that we don't handle these anymore), current batch size: {batchSize}...
    |]

  let alreadyHandledT =
        mkTableAliasToken @EraGlonassSynchronizedContract @"alreadyHandled"

  let alreadyHandledP = tableAliasTokenAliasProxy alreadyHandledT
  let alreadyHandledVinsOnlyP = Proxy @"alreadyHandledVinsOnly"

  let egSubProgramsT = mkTableAliasToken @SubProgram @"egSubPrograms"
  let egSubProgramsP = tableAliasTokenAliasProxy egSubProgramsT

  let -- | Few shared @SELECT@s of CTE.
      --
      -- 1. "alreadyHandledP"
      -- 2. "alreadyHandledVinsOnlyP"
      -- 3. "egSubProgramsP"
      sharedCTESelects :: [RawAliasAs' []]
      sharedCTESelects =
        [ -- VINs we're currently handling at the moment,
          -- which are currently marked as handled by CaRMa.
          -- Some of them we may have to "unmark" as handled by us,
          -- that's what we're going to find out.
          RawAliasAs' alreadyHandledP $
            let
              idField =
                mkTableAliasFieldToken
                  alreadyHandledT
                  EraGlonassSynchronizedContractId

              vinField =
                mkTableAliasFieldToken
                  alreadyHandledT
                  EraGlonassSynchronizedContractVin

              isHandledByCarmaField =
                mkTableAliasFieldToken
                  alreadyHandledT
                  EraGlonassSynchronizedContractIsHandledByCarma
            in
              [ rawSelect
                  [ RawTableAliasField idField
                  , RawTableAliasField vinField
                  ]
              , raw FROM
              ,   RawTableAlias alreadyHandledT
              , raw WHERE
              ,   rawAll
                    [ isHandledByCarmaField `rawEqual` True

                    -- -- Let's assume all VINs are correct and validated
                    -- -- when they appear in this table.
                    -- , rawMatchRegex
                    --     False
                    --     (RawTableAliasField vinField)
                    --     egVinPosixRegex
                    ]
              ]

        , RawAliasAs' alreadyHandledVinsOnlyP
            [ rawSelect $ pure $
                RawTableAliasField $
                  mkTableAliasFieldToken
                    alreadyHandledT
                    EraGlonassSynchronizedContractVin
            , raw FROM
            ,   RawAlias alreadyHandledP
            ]

        , -- @SubProgram@s which are marked as Era Glonass integration
          -- participants.
          RawAliasAs' egSubProgramsP $
            let
              idField = mkTableAliasFieldToken egSubProgramsT SubProgramId

              activeField =
                mkTableAliasFieldToken egSubProgramsT SubProgramActive

              egParticipantField =
                mkTableAliasFieldToken
                  egSubProgramsT
                  SubProgramEraGlonassParticipant

              parentProgramField =
                mkTableAliasFieldToken egSubProgramsT SubProgramParent
            in
              [ rawSelect $ pure $
                  RawTableAliasField idField
              , raw FROM
              ,   RawTableAlias egSubProgramsT
              , raw $ JOIN INNER
              ,   RawTable (Proxy @Program)
              , raw ON
              ,   rawAll
                    [ ProgramId     `rawEqualField` parentProgramField
                    , ProgramActive `rawEqual`      True
                    ]
              , raw WHERE
              ,   rawAll
                    [ activeField        `rawEqual` True
                    , egParticipantField `rawEqual` True
                    ]
              ]
        ]

  foundVINsToUnmark <-
    let
      inferTypes
        :: forall modelA modelB a b x
         . x ~ (Single VinToUnmarkModelId, Single EGVin)
        => (Proxy modelA, Proxy modelB)
        -> (EntityField modelA a, EntityField modelB b)
        -> ReaderT SqlBackend m [x]
        -> ReaderT SqlBackend m [x]

      inferTypes (Proxy, Proxy) (_, _) = id

      handledContractsT = mkTableAliasToken @Contract @"handledContracts"
      handledContractsP = tableAliasTokenAliasProxy handledContractsT
      handledContractsVinsOnlyP = Proxy @"handledContractsVinsOnly"
      validContractsP = Proxy @"validContracts"

      contractsToUnmarkAsHandledT =
        mkTableAliasToken @Contract @"contractsToUnmarkAsHandled"
      contractsToUnmarkAsHandledP =
        tableAliasTokenAliasProxy contractsToUnmarkAsHandledT

      ephemeralVINsToUnmarkAsHandledT =
        mkTableAliasToken
          @EraGlonassSynchronizedContract
          @"ephemeralVINsToUnmarkAsHandled"
      ephemeralVINsToUnmarkAsHandledP =
        tableAliasTokenAliasProxy ephemeralVINsToUnmarkAsHandledT
    in
      inferTypes (Proxy @Contract, Proxy @EraGlonassSynchronizedContract)
                 (ContractId,      EraGlonassSynchronizedContractId)
        $ uncurry rawEsqueletoSql
        $ buildRawSql
        $
        [ rawWith $
            sharedCTESelects
            <>
            [ -- @Contract@s which have been previously marked
              -- as handled by CaRMa.
              RawAliasAs' handledContractsP $
                let
                  vinFieldT =
                    mkTableAliasFieldToken handledContractsT ContractVin
                in
                  [ rawSelect $ pure $
                      raw STAR
                  , raw FROM
                  ,   RawTableAlias handledContractsT
                  , raw WHERE
                  ,   rawAll
                        [ rawIsNotNull vinFieldT
                        , vinFieldT `rawNotEqual` Just ""

                        -- -- Let's assume it's correct since it's presented
                        -- -- in @alreadyHandledVinsOnlyP@ table.
                        -- , rawMatchRegex
                        --     False
                        --     (rawFieldConstructor vinFieldT)
                        --     egVinPosixRegex

                        , -- @Contract@'s VIN is handled by CaRMa.
                          vinFieldT `rawIn`
                            rawSelectAlias alreadyHandledVinsOnlyP
                        ]
                  ]

            , RawAliasAs' handledContractsVinsOnlyP $
                [ rawSelect $ pure $
                    RawTableAliasField $
                      mkTableAliasFieldToken handledContractsT ContractVin
                , raw FROM
                ,   RawAlias handledContractsP
                ]

            , -- Few \"Contract"s may have the same VIN, so we check if a VIN is
              -- presented in this table we must not \"unmark" it.
              RawAliasAs' validContractsP $
                let
                  isActiveFieldT =
                    mkTableAliasFieldToken handledContractsT ContractIsActive

                  vinFieldT =
                    mkTableAliasFieldToken handledContractsT ContractVin

                  subProgramFieldT =
                    mkTableAliasFieldToken handledContractsT ContractSubprogram

                  validSinceFieldT =
                    mkTableAliasFieldToken handledContractsT ContractValidSince

                  validUntilFieldT =
                    mkTableAliasFieldToken handledContractsT ContractValidUntil
                in
                  [ rawSelectDistinctOn
                      [rawFieldConstructor vinFieldT]
                      [rawFieldConstructor vinFieldT]
                  , raw FROM
                  ,   RawAlias handledContractsP
                  , raw WHERE
                  ,   rawAll
                        [ isActiveFieldT `rawEqual` True
                        , subProgramFieldT `rawIn` rawSelectAlias egSubProgramsP

                        , rawSome
                            [ rawIsNull validSinceFieldT
                            , validSinceFieldT `rawLessOrEqual` Just nowDay
                            ]

                        , rawSome
                            [ rawIsNull validUntilFieldT
                            , validUntilFieldT `rawGreater` Just nowDay
                            ]
                        ]
                  ]

            , -- @Contract@s VINs of which we have to "unmark" as handled by
              -- CaRMa due to some of reasons you can find inside in the
              -- comments below.
              RawAliasAs' contractsToUnmarkAsHandledP $
                let
                  idFieldT =
                    mkTableAliasFieldToken handledContractsT ContractId

                  vinFieldT =
                    mkTableAliasFieldToken handledContractsT ContractVin

                  isActiveFieldT =
                    mkTableAliasFieldToken handledContractsT ContractIsActive

                  subProgramFieldT =
                    mkTableAliasFieldToken handledContractsT ContractSubprogram

                  validSinceFieldT =
                    mkTableAliasFieldToken handledContractsT ContractValidSince

                  validUntilFieldT =
                    mkTableAliasFieldToken handledContractsT ContractValidUntil
                in
                  [ rawSelect
                      [ RawTableAliasField idFieldT
                      , RawTableAliasField vinFieldT
                      ]
                  , raw FROM
                  ,   RawAlias handledContractsP
                  , raw WHERE
                  ,   rawAll
                        [ vinFieldT `rawNotIn` rawSelectAlias validContractsP
                        , rawSome
                            [ -- @Contract@ has been deactivated.
                              isActiveFieldT `rawEqual` False

                            , -- @SubProgram@ of a @Contract@ has been unmarked
                              -- as EG participant.
                              subProgramFieldT `rawNotIn`
                                rawSelectAlias egSubProgramsP

                            , -- Validity date of a @Contract@ isn't started yet.
                              rawAll
                                [ rawIsNotNull validSinceFieldT
                                , validSinceFieldT `rawGreater` Just nowDay
                                ]

                            , -- Validity date of a @Contract@ has been expired.
                              rawAll
                                [ rawIsNotNull validUntilFieldT
                                , validUntilFieldT `rawLessOrEqual` Just nowDay
                                ]
                            ]
                        ]
                  ]

            , -- Incorrect data (\"ephemeral VINs") when some VIN is handled by
              -- CaRMa but no longer represented in @Contract@s list (usually it
              -- doesn't happen but if for instance you change \"vin" field
              -- value of a @Contract@ and previous \"vin" have been marked as
              -- handled by CaRMa for EG service we supposed to notify EG that
              -- we don't handle these anymore)...
              RawAliasAs' ephemeralVINsToUnmarkAsHandledP $
                let
                  idFieldT =
                    mkTableAliasFieldToken
                      alreadyHandledT
                      EraGlonassSynchronizedContractId

                  vinFieldT =
                    mkTableAliasFieldToken
                      alreadyHandledT
                      EraGlonassSynchronizedContractVin
                in
                  [ rawSelect
                      [ RawTableAliasField idFieldT
                      , RawTableAliasField vinFieldT
                      ]
                  , raw FROM
                  ,   RawAlias alreadyHandledP
                  , raw WHERE
                  ,   vinFieldT `rawNotIn`
                        rawSelectAlias handledContractsVinsOnlyP
                  ]
            ]
        , rawSelect $
            let
              contractIdField =
                mkTableAliasFieldToken contractsToUnmarkAsHandledT ContractId

              contractVinField =
                mkTableAliasFieldToken contractsToUnmarkAsHandledT ContractVin

              egSyncContractIdField =
                mkTableAliasFieldToken
                  ephemeralVINsToUnmarkAsHandledT
                  EraGlonassSynchronizedContractId

              egSyncContractVinField =
                mkTableAliasFieldToken
                  ephemeralVINsToUnmarkAsHandledT
                  EraGlonassSynchronizedContractVin

              fieldBranch x = (rawIsNotNull' x, RawTableAliasField x)

              idFieldBranch
                :: forall model alias c
                 .
                 ( KnownSymbol <=> '[ c, alias ]
                 , RawPieceConstraint []
                 , ConstructorName (Rep VinToUnmarkModelId) c ~ 'Just c
                 , OneOf model '[ Contract, EraGlonassSynchronizedContract ]
                 , PersistEntity model
                 , Typeable model

                 , If (c == "ContractIdToUnmark")
                      (model ~ Contract)
                      (c ~ "EraGlonassSynchronizedContractIdToUnmark")

                 , If (c == "EraGlonassSynchronizedContractIdToUnmark")
                      (model ~ EraGlonassSynchronizedContract)
                      (c ~ "ContractIdToUnmark")
                 )
                => TableAliasFieldToken model alias (Key model)
                -> Proxy c
                -> (RawSqlPiece [], RawSqlPiece [])

              idFieldBranch x p@Proxy =
                ( rawIsNotNull' x
                , rawConcat [ RawValue $ symbolVal p `mappend` " "
                            , RawTableAliasField x
                            ]
                )
            in
              [ rawBranching
                  ( idFieldBranch
                      contractIdField
                      (Proxy @"ContractIdToUnmark")
                  )
                  [ idFieldBranch
                      egSyncContractIdField
                      (Proxy @"EraGlonassSynchronizedContractIdToUnmark")
                  ]
                  Nothing

              , rawBranching
                  (fieldBranch contractVinField)
                  [fieldBranch egSyncContractVinField]
                  Nothing
              ]
        , raw FROM
        ,   RawAlias contractsToUnmarkAsHandledP
        , raw $ JOIN FULL
        ,   RawAlias ephemeralVINsToUnmarkAsHandledP
        , raw ON
        ,   RawValue True
        , rawLimitTo batchSize
        ]

  let vinsToUnmarkData = go where
        go = foldl reducer defaultVINsToUnmark foundVINsToUnmark
        reducer z@VINsToUnmark {..} = \case
          (Single (ContractIdToUnmark x), Single y) -> z
            { totalVINsToUnmarkCount    = succ totalVINsToUnmarkCount
            , contractVINsToUnmarkCount = succ contractVINsToUnmarkCount
            , contractVINsToUnmark      = (x, y) : contractVINsToUnmark
            }
          (Single (EraGlonassSynchronizedContractIdToUnmark x), Single y) -> z
            { totalVINsToUnmarkCount     = succ totalVINsToUnmarkCount
            , ephemeralVINsToUnmarkCount = succ ephemeralVINsToUnmarkCount
            , ephemeralVINsToUnmark      = (x, y) : ephemeralVINsToUnmark
            }

  let vinsToUnmark
        :: Maybe
         ( OneOrTwoNonEmptyLists
             (ContractId, EGVin)
             (EraGlonassSynchronizedContractId, EGVin)
         )

      vinsToUnmark =
        case ( contractVINsToUnmark  vinsToUnmarkData
             , ephemeralVINsToUnmark vinsToUnmarkData
             ) of
             (x : xs, y : ys) -> Just $ BothNonEmptyLists  (x :| xs) (y :| ys)
             (x : xs, []    ) -> Just $ FirstNonEmptyList  (x :| xs)
             ([],     x : xs) -> Just $ SecondNonEmptyList (x :| xs)
             ([],     []    ) -> Nothing

  case vinsToUnmark of
       Just vinsToUnmark' -> do
         srcLogDebug $
           let contract = typeName (Proxy :: Proxy Contract) :: Text in [qmb|
             There are some VINs to unmark as handled by CaRMa first:
             \  Current batch size limit: {batchSize}
             \  Total: {totalVINsToUnmarkCount vinsToUnmarkData}
             \  Outdated/deactivated VINs count: \
                  {contractVINsToUnmarkCount vinsToUnmarkData}
             \  Ephemeral VINs \
                (not represented in "{contract}"s anymore) count: \
                  {ephemeralVINsToUnmarkCount vinsToUnmarkData}
           |]

         unmarkAsHandled vinsToUnmark'
           ( totalVINsToUnmarkCount     vinsToUnmarkData
           , contractVINsToUnmarkCount  vinsToUnmarkData
           , ephemeralVINsToUnmarkCount vinsToUnmarkData
           )

         if totalVINsToUnmarkCount vinsToUnmarkData < batchSize
            then
              srcLogDebug [qns|
                Done with unmarking some VINs as handled by CaRMa, so, commiting
                current database transaction and repeating synchronization
                (repeating whole operation to cover rare but technically
                possible cases when after transaction is commited some
                subprogram stopped being an Era Glonass participant for
                instance)...
              |]
            else
              srcLogDebug [qms|
                Total count ({totalVINsToUnmarkCount vinsToUnmarkData}) of VINs
                to "unmark" isn't less than batch size limit ({batchSize}) it
                means there are probably more VINs to "unmark" as handled by
                CaRMa, so, repeating whole operation for next batch iteration...
              |]

         -- We're repeating whole operation anyway, in both cases
         -- (see condition above).
         repeatWholeOperation

       Nothing -> do
         vinsToSynchronize <-
           let
             inferTypes
               :: forall modelA a x. x ~ (Single ContractId, Single EGVin)
               => Proxy modelA
               -> EntityField modelA a
               -> ReaderT SqlBackend m [x]
               -> ReaderT SqlBackend m [x]

             inferTypes Proxy _ = id
           in
             inferTypes (Proxy @Contract) ContractId
               $ uncurry rawEsqueletoSql
               $ buildRawSql
               $
               let
                 contractsT = mkTableAliasToken @Contract @"contracts"
                 idFieldT   = mkTableAliasFieldToken contractsT ContractId
                 vinFieldT  = mkTableAliasFieldToken contractsT ContractVin

                 isActiveFieldT =
                   mkTableAliasFieldToken contractsT ContractIsActive

                 subProgramFieldT =
                   mkTableAliasFieldToken contractsT ContractSubprogram

                 validSinceFieldT =
                   mkTableAliasFieldToken contractsT ContractValidSince

                 validUntilFieldT =
                   mkTableAliasFieldToken contractsT ContractValidUntil

                 fieldsToSelect =
                   [ rawFieldConstructor idFieldT
                   , rawFieldConstructor vinFieldT
                   ]

                 -- | Nested inside parent DISTINCT selection.
                 mainSelection = RawWrap
                   [ rawSelect
                       fieldsToSelect
                   , raw FROM
                   ,   RawTableAlias contractsT
                   , raw WHERE
                   ,   rawAll
                         [ isActiveFieldT `rawEqual` True

                         , subProgramFieldT `rawIn`
                             rawSelectAlias egSubProgramsP

                         , rawSome
                             [ rawIsNull validSinceFieldT
                             , validSinceFieldT `rawLessOrEqual` Just nowDay
                             ]

                         , rawSome
                             [ rawIsNull validUntilFieldT
                             , validUntilFieldT `rawGreater` Just nowDay
                             ]

                         , rawIsNotNull vinFieldT
                         , vinFieldT `rawNotEqual` Just ""

                         , -- Some @Contract@s may have incorrect VIN field.
                           rawMatchRegex
                             False
                             (rawFieldConstructor vinFieldT)
                             egVinPosixRegex

                         , vinFieldT `rawNotIn`
                             rawSelectAlias alreadyHandledVinsOnlyP
                         ]
                   , rawOrderBy [Descending validSinceFieldT]
                   ]
               in
                 [ rawWith
                     sharedCTESelects
                 , rawSelectDistinctOn
                     [rawFieldConstructor vinFieldT]
                     fieldsToSelect
                 , raw FROM
                 ,   mainSelection
                 ,     raw AS
                 ,       RawAlias $ tableAliasTokenAliasProxy contractsT
                 , rawLimitTo batchSize
                 ]

         let logMsg (foundSmth :: Bool) (count' :: Word) = msg where
               contract   = typeName (Proxy @Contract)   :: Text
               subProgram = typeName (Proxy @SubProgram) :: Text

               msg = [qms|
                 There are no VINs to "unmark" as handled by CaRMa,
                 {if foundSmth then synchronizing else nothingElseToDo}
               |] :: Text

               synchronizing = [qms|
                 continuing to synchronize found {count'} "{contract}"s (marking
                 some VINs as handled by us which belong to Era Glonass
                 participant "{subProgram}")...
               |] :: Text

               nothingElseToDo = [qms|
                 and no "{contract}"s found to synchronize, we're done for now.
               |] :: Text

         case vinsToSynchronize of
              [] -> doneLog << srcLogDebug (logMsg False minBound)

              x : xs -> do
                let vinsToSynchronizeCount =
                      fromIntegral $ length vinsToSynchronize

                srcLogDebug $ logMsg True vinsToSynchronizeCount
                synchronizeContracts (x :| xs) vinsToSynchronizeCount

                if vinsToSynchronizeCount < batchSize
                   then doneLog
                   else repeatWholeOperation << srcLogDebug [qms|
                          Total count ({vinsToSynchronizeCount}) of VINs to
                          synchronize isn't less than batch size limit
                          ({batchSize}) it means there are probably more VINs to
                          mark as handled by CaRMa, so, repeating whole
                          operation for next batch iteration...
                        |]
