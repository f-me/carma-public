{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, LambdaCase #-}
{-# LANGUAGE QuasiQuotes, ViewPatterns, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances, FlexibleContexts #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | VIN synchronizer worker module.
module Carma.EraGlonass.VinSynchronizer
     ( runVinSynchronizer
     ) where

import           Prelude hiding (fail)

import           Data.Proxy
import           Data.Function (fix)
import           Data.Time.Clock (utctDay)
import           Data.Time.LocalTime (TimeZone)
import           Data.Text (Text)
import           Text.InterpolatedString.QM
import           Text.Printf (printf)
import           Data.Maybe (catMaybes)
import           Data.List (intersect)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Aeson (Value)

import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Reader (MonadReader, ReaderT, asks)
import           Control.Monad.Catch (MonadCatch, MonadThrow (throwM))
import           Control.Exception.Base (AssertionFailed (AssertionFailed))

import           Control.Exception
                   ( SomeException (SomeException)
                   , fromException
                   , displayException
                   )

import           Database.Persist ((==.), (!=.), (>=.), (<-.), (/<-.), (||.))
import           Database.Persist.Types (Entity (..))
import           Database.Persist.Sql (SqlBackend)

import           Carma.Monad
import           Carma.Model.Contract.Persistent
import           Carma.Model.SubProgram.Persistent
import           Carma.Utils.Operators
import           Carma.Utils.TypeSafe.Generic.DataType
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Instance.Persistent (TimeoutException (..))
import           Carma.EraGlonass.Helpers
import           Carma.EraGlonass.Model.EraGlonassSynchronizedContract.Persistent
import           Carma.EraGlonass.Model.CaseEraGlonassFailure.Persistent
import           Carma.EraGlonass.VinSynchronizer.Types
import           Carma.EraGlonass.VinSynchronizer.Helpers
import           Carma.EraGlonass.VinSynchronizer.UnmarkAsHandled
import           Carma.EraGlonass.VinSynchronizer.SynchronizeContracts
import           Carma.EraGlonass.Types.AppContext (AppContext (..))
import           Carma.EraGlonass.Types.EGIntegrationPoint
                   ( EGIntegrationPoint (BindVehicles)
                   )


-- | VIN synchronizer worker starter.
runVinSynchronizer
  ::
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadClock m
   , MonadDelay m -- To wait before synchronizations.
   , MonadPersistentSql m
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
    {printf "%.2f" (inHours interval) :: String} hour(s).
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
        before 00:00 to trigger next VIN synchronization...
      |]

      delay $ round $ hoursToWait * 3600 * (10 ** 6)
      srcLogDebug [qns| It's about 00:00,
                        initiating VIN synchronization process... |]

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

  synchronizationResolve
    (Left (fromException -> Just (TimeoutExceeded n))) = False <$ do
      retryInterval <- asks vinSynchronizerRetryInterval

      let failureMessage = [qms|
            Synchronization of VINs is failed becuase it is exceeded timeout of
            {round $ (fromIntegral n / 1000 / 1000 :: Float) :: Int} second(s).
          |] :: Text

      srcLogError [qm| {failureMessage} {willBeRetried retryInterval} |]
      currentTime <- getCurrentTime
      saveFailureIncidentInBackground currentTime failureMessage Nothing
      delay retryInterval
      srcLogInfo $ retrying retryInterval

  synchronizationResolve
    -- Only response is possible to fail to parse since we don't have incoming
    -- requests in this integration point.
    (Left (fromException -> Just (ResponseParseFailure err body))) = False <$ do
      retryInterval <- asks vinSynchronizerRetryInterval

      let failureMessage = [qms|
            Synchronization of VINs is failed becuase response from EG service
            is failed to parse with this error message: {err}
          |] :: Text

      srcLogError [qm| {failureMessage} {willBeRetried retryInterval} |]
      currentTime <- getCurrentTime
      saveFailureIncidentInBackground currentTime failureMessage (Just body)
      delay retryInterval
      srcLogInfo $ retrying retryInterval

  synchronizationResolve
    (Left (fromException -> Just (FailureScenario err))) = False <$ do
      retryInterval <- asks vinSynchronizerRetryInterval

      let failureMessage = [qms|
            Synchronization of VINs is failed becuase EG service
            responded with failure case: {err}
          |] :: Text

      srcLogError [qm| {failureMessage} {willBeRetried retryInterval} |]
      currentTime <- getCurrentTime
      saveFailureIncidentInBackground currentTime failureMessage Nothing
      delay retryInterval
      srcLogInfo $ retrying retryInterval

  synchronizationResolve (Left exception) = False <$ do
    retryInterval <- asks vinSynchronizerRetryInterval

    let failureMessage = [qms|
          Synchronization of VINs is failed with exception: {exception}.
        |] :: Text

    srcLogError [qm| {failureMessage} {willBeRetried retryInterval} |]
    currentTime <- getCurrentTime
    saveFailureIncidentInBackground currentTime failureMessage Nothing
    delay retryInterval
    srcLogInfo $ retrying retryInterval


-- | VINs synchronization logic handler.
--
-- If you don't see comments for code just look at log messages nearby,
-- it may tell you a lot instead.
synchronizeVins
  ::
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadPersistentSql m
   , MonadServantClient m
   , MonadClock m
   , MonadThrow m
   )
  => ReaderT SqlBackend m ()

synchronizeVins =
  (srcLogInfo [qn| Synchronizing VINs... |] >>) $ fix $ \((

    let
      preLog = srcLogDebug [qns|
        Committing VIN synchronization transaction
        before running whole operation again...
      |]

      postLog = srcLogDebug [qn| Running VIN synchronization again... |]
    in
      (preLog >> transactionSave >> postLog >>)

  ) -> repeatWholeOperation) -> do

  handledContracts <- do
    srcLogDebug [qns|
      Getting list of currently handled VINs to check if some of them
      should be marked for EG service as not handled by CaRMa anymore...
    |]

    selectList [ EraGlonassSynchronizedContractIsHandledByCarma ==. True ] []

  egSubPrograms <- do
    srcLogDebug $
      let subProgram = typeName (Proxy :: Proxy SubProgram) :: Text
       in [qm| Getting list of active EG participants "{subProgram}"s... |]

    selectKeysList [ SubProgramActive                ==. True
                   , SubProgramEraGlonassParticipant ==. True
                   ] []

  nowDay <- utctDay <$> lift getCurrentTime

  let -- | Prepared to be used in DB requests list of VINs.
      handledVINs :: [Text]
      handledVINs =
        handledContracts <&> eraGlonassSynchronizedContractVin . entityVal

  contractsToUnmarkAsHandled <- do
    srcLogDebug $
      let contract = typeName (Proxy :: Proxy Contract) :: Text in [qms|
        Getting list of "{contract}"s (VINs actually) which are used to be
        handled by us (now we need to notify EG service that they
        aren't handled by CaRMa anymore)...
      |]

    flip selectList []
      $ -- @Contract@'s VIN is handled by CaRMa.
        ( ContractVin <-. fmap Just handledVINs )

      : (   -- @Contract@ has been deactivated.
            [ ContractIsActive ==. False ]

        ||. -- @SubProgram@ of a @Contract@ has been unmarked
            -- as EG participant.
            [ ContractSubprogram /<-. fmap Just egSubPrograms ]

        ||. -- Validity date of a @Contract@ has been expired.
            [ ContractValidUntil !=. Nothing
            , ContractValidUntil >=. Just nowDay
            ]
        )

  ephemeralVINsToUnmarkAsHandled <- do
    srcLogDebug $
      let contract = typeName (Proxy :: Proxy Contract) :: Text in [qms|
        Searching for incorrect data ("ephemeral VINs"), when some VIN is
        handled by CaRMa but no longer represented in "{contract}"s list
        (usually it doesn't happen but if for instance you change "vin" field
        value of a "{contract}" and previous "vin" have been marked as
        handled by CaRMa for EG service we supposed to notify EG that we don't
        handle these anymore)...
      |]

    selectList [ ContractVin <-. fmap Just handledVINs ] []
      <&> fmap (entityVal ? contractVin) ? catMaybes -- Extracting only VINs
      <&> \contractVINs ->
            let vinLens = entityVal ? eraGlonassSynchronizedContractVin

                f acc x =
                  if vinLens x `notElem` contractVINs
                     then x : acc
                     else acc

                in foldl f [] handledContracts

  do
    srcLogDebug [qns|
      Checking for data correctness
      ("ephemeral VINs" and outdated/deactivated VINs cannot intersect)...
    |]

    let vinsA
           =  catMaybes
           $  contractVin . entityVal
          <$> contractsToUnmarkAsHandled

    let vinsB
           =  eraGlonassSynchronizedContractVin . entityVal
          <$> ephemeralVINsToUnmarkAsHandled

    unless (null $ vinsA `intersect` vinsB) $
      throwM $ AssertionFailed [qns|
        Ephemeral VINs and outdated/deactivated VINs cannot intersect,
        something went wrong...
      |]

  let vinsToUnmark
        :: Maybe
         ( OneOrTwoNonEmptyLists
             (Entity Contract)
             (Entity EraGlonassSynchronizedContract)
         )

      vinsToUnmark =
        case (contractsToUnmarkAsHandled, ephemeralVINsToUnmarkAsHandled) of
             (x : xs, y : ys) -> Just $ BothNonEmptyLists  (x :| xs) (y :| ys)
             (x : xs, []    ) -> Just $ FirstNonEmptyList  (x :| xs)
             ([],     x : xs) -> Just $ SecondNonEmptyList (x :| xs)
             ([],     []    ) -> Nothing

  case vinsToUnmark of
       Just vinsToUnmark' -> do
         srcLogDebug $
           let contract = typeName (Proxy :: Proxy Contract) :: Text in [qmb|
             There's some VINs to unmark as handled by CaRMa first:
             \  Outdated/deactivated VINs count: \
                  { case vinsToUnmark' of
                         FirstNonEmptyList  y   -> length y
                         SecondNonEmptyList _   -> 0
                         BothNonEmptyLists  y _ -> length y
                  }
             \  Ephemeral VINs \
                (not represented in "{contract}"s anymore) count: \
                  { case vinsToUnmark' of
                         FirstNonEmptyList  _   -> 0
                         SecondNonEmptyList y   -> length y
                         BothNonEmptyLists  _ y -> length y
                  }
           |]

         unmarkAsHandled vinsToUnmark'

         srcLogDebug [qns|
           Done with unmarking some VINs as handled by CaRMa, so, commiting
           current database transaction and repeating synchronization
           (repeating whole operation to cover rare but technically possible
           cases when after transaction is commited some subprogram stopped
           being an Era Glonass participant for instance)...
         |]

         repeatWholeOperation

       Nothing ->
         let
           logMsg hasEGSubProgram = msg where
             contract   = typeName (Proxy :: Proxy Contract)   :: Text
             subProgram = typeName (Proxy :: Proxy SubProgram) :: Text

             msg = [qms|
               There are no VINs to "unmark" as handled by CaRMa,
               {if hasEGSubProgram then synchronizing else nothingElseToDo}
             |] :: Text

             synchronizing = [qms|
               continuing to synchronize "{contract}"s (marking some VINs as
               handled by us which belong to Era Glonass participant
               "{subProgram}")...
             |] :: Text

             nothingElseToDo = [qms|
               and also there are no active Era Glonass participant
               "{subProgram}"s, we have nothing to synchronize,
               so we're done for now.
             |] :: Text
         in
           case egSubPrograms of
                [] -> srcLogDebug $ logMsg False
                x : xs -> do
                  srcLogDebug $ logMsg True
                  synchronizeContracts nowDay handledContracts $ x :| xs


saveFailureIncidentInBackground
  :: forall m
   .
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadCatch m
   , MonadThread m
   , MonadPersistentSql m
   , MonadSTM m
   )
  => UTCTime
  -> Text
  -> Maybe Value -- ^ Response body which failed to parse.
  -> m ()

saveFailureIncidentInBackground ctime comment responseBody = go where
  go = inBackground $ runSqlInTime (insert_ record) >>= resolve :: m ()

  record =
    CaseEraGlonassFailure
      { caseEraGlonassFailureCtime            = ctime
      , caseEraGlonassFailureIntegrationPoint = BindVehicles
      , caseEraGlonassFailureRequestId        = Nothing
      , caseEraGlonassFailureRequestBody      = Nothing
      , caseEraGlonassFailureResponseBody     = responseBody
      , caseEraGlonassFailureComment          = Just comment
      }

  resolve (Right ()) = pure ()
  resolve (Left (SomeException e)) = srcLogError [qms|
    Failed to save failure incident record into database,
    it is failed with exception: {displayException e}.
  |]
