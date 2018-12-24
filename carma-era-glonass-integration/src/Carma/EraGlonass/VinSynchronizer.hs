{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, RecordWildCards #-}
{-# LANGUAGE QuasiQuotes, ViewPatterns, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | VIN synchronizer worker module.
--
-- Also known as integration point __CRM.EG.02__.
module Carma.EraGlonass.VinSynchronizer
     ( runVinSynchronizer
     ) where

import           Data.Typeable
import           Data.Function (fix)
import           Data.Time.Clock (utctDay)
import           Data.Time.LocalTime (TimeZone, utcToZonedTime)
import           Data.Time.Format
import           Data.Text (Text)
import           Text.InterpolatedString.QM
import           Text.Printf (printf)
import           Data.Maybe (catMaybes)
import           Data.List (intersect)
import           Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.Vector as Vec
import           Data.Aeson

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Reader (MonadReader, ReaderT, asks)
import           Control.Monad.Catch
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Random.Class (MonadRandom)

import           Database.Persist ((==.), (!=.), (>=.), (<-.), (/<-.), (||.))
import           Database.Persist.Sql (SqlBackend, fromSqlKey)
import           Database.Persist.Types (Entity (..))

import           Carma.Utils.Operators
import           Carma.Monad
import           Carma.Model.SubProgram.Persistent
import           Carma.Model.Contract.Persistent
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Instance.Persistent (TimeoutException (..))
import           Carma.EraGlonass.Types
import           Carma.EraGlonass.Model.CaseEraGlonassFailure.Types
import           Carma.EraGlonass.Model.EraGlonassSynchronizedContract.Persistent
import           Carma.EraGlonass.Client


-- | VIN synchronizer monad constraint.
type VinSynchronizerMonad m =
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadClock m
   , MonadDelay m
   , MonadPersistentSql m
   , MonadThrow m
   , MonadRandom m -- For creating new @RequestId@
   , MonadConcurrently m
   , MonadServantClient m
   )


-- | VIN synchronizer worker starter.
runVinSynchronizer :: VinSynchronizerMonad m => TimeZone -> m ()
runVinSynchronizer tz = do
  logInfo [qm| Running VIN synchronizer worker (timezone is {tz})... |]

  forever $ do
    (hoursToWait, minutesToWait) <- getTimeToWait tz

    logDebug $ let zeroPad x = printf "%02d" (x :: Word) :: String in [qms|
      Waiting for {zeroPad $ floor hoursToWait}:{zeroPad minutesToWait}
      before 00:00 to trigger next VIN synchronization...
    |]

    -- delay $ round $ hoursToWait * 3600 * (10 ** 6) -- TODO uncomment
    delay $ 3 * (10 ^ (6 :: Int)) -- TODO remove (for testing purposes)
    logDebug [qn| It's about 00:00, initiating VIN synchronization process... |]

    fix $ \again ->
      asks vinSynchronizerTimeout
        >>= flip runSqlTimeout synchronizeVins
        >>= synchronizationResolve
        >>= flip unless again

  where
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


    -- | Returned @Bool@ indicates successfulness status.
    --
    -- @True@ means it is done, @False@ means it is failed.
    --
    -- TODO Report about failures to @CaseEraGlonassFailure@ model.
    synchronizationResolve
      :: ( MonadReader AppContext m
         , MonadLoggerBus m
         , MonadDelay m
         )
      => Either SomeException ()
      -> m Bool

    synchronizationResolve (Right ()) = True <$
      logInfo [qn|
        VINs synchronization iteration is finished successfully.
      |]

    synchronizationResolve ( Left (fromException -> Just (TimeoutExceeded n))
                           ) = False <$ do

      retryInterval <- asks vinSynchronizerRetryInterval

      logError [qms|
        Synchronization of VINs is failed becuase it is exceeded timeout of
          {round $ (fromIntegral n / 1000 / 1000 :: Float) :: Int} second(s).
        {willBeRetried retryInterval}
      |]

      delay retryInterval
      logInfo $ retrying retryInterval

    synchronizationResolve (Left exception) = False <$ do
      retryInterval <- asks vinSynchronizerRetryInterval

      logError [qms|
        Synchronization of VINs is failed with exception: {exception}.
        {willBeRetried retryInterval}
      |]

      delay retryInterval
      logInfo $ retrying retryInterval


-- | VINs synchronization logic handler.
--
-- Instead of comments you could just read log messages added all over the
-- place.
synchronizeVins :: VinSynchronizerMonad m => ReaderT SqlBackend m ()
synchronizeVins = do
  logInfo [qn| Synchronizing VINs... |]

  fix $ \(( let preLog = logDebug [qns|
                  Committing VIN synchronization transaction
                  before running it again...
                |]

                postLog = logDebug [qn| Running VIN synchronization again... |]

                in (preLog >> transactionSave >> postLog >>)

          ) -> runAgain) -> do

    handledContracts <- do
      logDebug [qns|
        Getting list of currently handled VINs to check if some of them
        should be marked for EG service as not handled by CaRMa anymore...
      |]

      selectList [ EraGlonassSynchronizedContractIsHandledByCarma ==. True ] []

    egSubPrograms <- do
      logDebug [qn| Getting list of active EG participants "SubProgram"s... |]
      selectKeysList [ SubProgramActive                ==. True
                     , SubProgramEraGlonassParticipant ==. True
                     ] []

    nowDay <- utctDay <$> lift getCurrentTime

    let handledVINs :: [Text]
        handledVINs =
          handledContracts <&> eraGlonassSynchronizedContractVin . entityVal

    contractsToUnmarkAsHandled <- do
      logDebug [qns|
        Getting list of "Contract"s (VINs actually) which are used to be handled
        (now we need to notify EG service that they aren't handled by CaRMa
        anymore)...
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
      logDebug [qns|
        Searching for incorrect data ("ephemeral VINs"), when some VIN is
        handled by CaRMa but no longer represented in "Contract"s list
        (usually it doesn't happen but if for instance you change "vin" field
        value of a "Contract" and previous "vin" have been marked as handled
        by CaRMa for EG service we supposed to notify EG that we don't handle
        these anymore)...
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
      logDebug [qns|
        Checking for data correctness
        ("ephemeral VINs" and outdated/deactivated VINs cannot intersect)...
      |]

      unless ( let vinsA =
                     catMaybes $
                       contractsToUnmarkAsHandled <&> entityVal ? contractVin

                   vinsB =
                     ephemeralVINsToUnmarkAsHandled <&>
                       entityVal ? eraGlonassSynchronizedContractVin

                   in null $ vinsA `intersect` vinsB ) $

        fail [qns|
          Ephemeral VINs and outdated/deactivated VINs cannot intersect,
          something is wrong...
        |]

    let vinsToUnmark
          :: Maybe ( OneOrTwoNonEmptyLists
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
         Just x -> do
           logDebug [qmb|
             There's some VINs to unmark as handled by CaRMa first:
             \  Outdated/deactivated VINs count: \
                  { case x of
                         FirstNonEmptyList  y   -> length y
                         SecondNonEmptyList _   -> 0
                         BothNonEmptyLists  y _ -> length y
                  }
             \  Ephemeral VINs (not represented in "Contract"s anymore) count: \
                  { case x of
                         FirstNonEmptyList  _   -> 0
                         SecondNonEmptyList y   -> length y
                         BothNonEmptyLists  _ y -> length y
                  }
           |]

           unmarkAsHandled x

           logDebug [qns|
             Done with unmarking some VINs as handled by CaRMa, so,
             running whole VIN synchronization operation again...
           |]

           runAgain

         Nothing -> do
           logDebug [qn|
             There's no VINs to unmark as handled by CaRMa, so, continuing...
           |]

           foo

  where
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
          logDebug [qns|
            Checking status of CaRMa outdated/deactivated "Contract"s
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
                                VIN of a "Contract" with id
                                {fromSqlKey entityKey} is incorrect.
                                Error message: {msg}
                              |]

                       Nothing -> Left [qms|
                         VIN of a "Contract" with id {fromSqlKey entityKey}
                         could not be "Nothing", it supposed to be checked
                         earlier.
                       |]

          requestsList <- case requests of
            Right x -> pure x
            Left  e ->
              logError [qms|
                {CrmEg02} is failed on constructing request to check status of
                CaRMa outdated/deactivated "Contract"s.
                Error: {e}
              |] >> fail e

          -- Checking for data correctness
          uncurry when $
            let
              lenContracts = length entities
              lenRequests  = length requestsList

              failMsg = [qmb|
                Constructing request to check status of \
                CaRMa outdated/deactivated "Contract"s is failed.
                Unexpectedly count of "Contract"s is not equal \
                to count of requests (VINs to check).
                \  Count of "Contract"s is {lenContracts}.
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

          let requestResolver (Right x) = pure x
              requestResolver (Left  e) = do
                logError [qms|
                  {CrmEg02} is failed on POST request to Era Glonass
                  to check status of CaRMa outdated/deactivated "Contract"s.
                  Error: {e}
                |]

                throwM $ EGCheckVinRequestIsFailed e egRequestData

          let responseResolver EGCheckVinResponseIncorrect {..} = do
                logError [qmb|
                  {CrmEg02} is failed on parsing response data \
                  from Era Glonass to check status \
                  of CaRMa outdated/deactivated "Contract"s.
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
                      CaRMa outdated/deactivated "Contract"s.
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
                      CaRMa outdated/deactivated "Contract"s.
                      Unexpectedly a VIN from response is not equal to one
                      from request (at the same position in order).
                      \  Contract id is {fromSqlKey contractId}.
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
                         VIN of a "EraGlonassSynchronizedContract" with id
                         {fromSqlKey entityKey} is incorrect
                         (which is really strange,
                         it supposed to be validated earlier).
                         Error message: {msg}
                       |]

          requestsList <- case requests of
            Right x -> pure x
            Left  e ->
              logError [qms|
                Failed to construct request
                to check status of CaRMa "ephemeral VINs".
                Error: {e}
              |] >> fail e

          reqId <- newRequestId

          let egRequestData =
                EGCheckVinRequest
                  { requestId = reqId
                  , requests  = requestsList
                  }

          -- TODO commit request
          egRequestData `seq` pure []

    foo :: VinSynchronizerMonad m => ReaderT SqlBackend m ()
    foo = fail "TODO implement"


{-|
Returns hours and minutes to wait before triggering next VIN synchronization.

Minutes already included into hours as fractional part.
-}
getTimeToWait :: MonadClock m => TimeZone -> m (Float, Word)
getTimeToWait tz = getCurrentTime <&> utcToZonedTime tz ? f
  where
    format = formatTime defaultTimeLocale
    readFloat = read :: String -> Float
    split = id &&& id

    f =
      format "%H" &&& format "%M" -- Getting hours and minutes
      >>> readFloat *** readFloat
      >>> second (/ 60) -- Minutes to fractional part of an hour

      >>> -- Sum hours and fractional part of an hour (minutes),
          -- getting hours left to 00:00 (with fractional part),
          -- then split united result.
          arr (uncurry (+) ? (`subtract` 24) ? split)

      >>> -- Getting minutes left (as a remainder apart from hours).
          second ( (properFraction :: Float -> (Int, Float))
                 ? snd    -- Only remainder
                 ? (* 60) -- Back to real minutes
                 ? round
                 )


data EGRequestException
   = EGCheckVinRequestIsFailed ServantError EGCheckVinRequest
     -- ^ When request to EG is failed
   | EGCheckVinResponseIsFailed String Value
     -- ^ When for instance parsing response from EG is failed
     deriving (Show, Eq, Typeable)

instance Exception EGRequestException


-- | Helper type for VINs unmarking.
data OneOrTwoNonEmptyLists first second
   = FirstNonEmptyList  (NonEmpty first)
   | SecondNonEmptyList (NonEmpty second)
   | BothNonEmptyLists  (NonEmpty first) (NonEmpty second)
     deriving (Show, Eq)


getFirstNonEmptyList :: OneOrTwoNonEmptyLists a b -> Maybe (NonEmpty a)
getFirstNonEmptyList (FirstNonEmptyList  x  ) = Just x
getFirstNonEmptyList (SecondNonEmptyList _  ) = Nothing
getFirstNonEmptyList (BothNonEmptyLists  x _) = Just x

getSecondNonEmptyList :: OneOrTwoNonEmptyLists a b -> Maybe (NonEmpty b)
getSecondNonEmptyList (FirstNonEmptyList  _  ) = Nothing
getSecondNonEmptyList (SecondNonEmptyList x  ) = Just x
getSecondNonEmptyList (BothNonEmptyLists  _ x) = Just x
