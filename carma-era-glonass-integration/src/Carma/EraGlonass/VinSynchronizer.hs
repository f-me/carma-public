{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes, ViewPatterns, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances, FlexibleContexts #-}

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
import           Data.Time.LocalTime (TimeZone)
import           Data.Text (Text)
import           Text.InterpolatedString.QM
import           Text.Printf (printf)
import           Data.Maybe (catMaybes)
import           Data.List (intersect)
import           Data.List.NonEmpty (NonEmpty (..))

import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Reader (MonadReader, ReaderT, asks)
import           Control.Monad.Catch

import           Database.Persist ((==.), (!=.), (>=.), (<-.), (/<-.), (||.))
import           Database.Persist.Sql (SqlBackend)
import           Database.Persist.Types (Entity (..))

import           Carma.Utils.Operators
import           Carma.Monad
import           Carma.Model.SubProgram.Persistent
import           Carma.Model.Contract.Persistent
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Instance.Persistent (TimeoutException (..))
import           Carma.EraGlonass.Types
import           Carma.EraGlonass.Model.EraGlonassSynchronizedContract.Persistent
import           Carma.EraGlonass.VinSynchronizer.Types
import           Carma.EraGlonass.VinSynchronizer.Helpers
import           Carma.EraGlonass.VinSynchronizer.UnmarkAsHandled
import           Carma.EraGlonass.VinSynchronizer.SynchronizeContracts


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
      logInfo [qn| VINs synchronization iteration is finished successfully.  |]

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
      logDebug [qms|
        Getting list of active EG participants
        "{typeRep (Proxy :: Proxy SubProgram)}"s...
      |]

      selectKeysList [ SubProgramActive                ==. True
                     , SubProgramEraGlonassParticipant ==. True
                     ] []

    nowDay <- utctDay <$> lift getCurrentTime

    let handledVINs :: [Text]
        handledVINs =
          handledContracts <&> eraGlonassSynchronizedContractVin . entityVal

    contractsToUnmarkAsHandled <- do
      logDebug [qms|
        Getting list of "{contractTypeRep}"s (VINs actually) which are used to
        be handled (now we need to notify EG service that they aren't handled by
        CaRMa anymore)...
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
      logDebug [qms|
        Searching for incorrect data ("ephemeral VINs"), when some VIN is
        handled by CaRMa but no longer represented in "{contractTypeRep}"s list
        (usually it doesn't happen but if for instance you change "vin" field
        value of a "{contractTypeRep}" and previous "vin" have been marked as
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
             \  Ephemeral VINs \
                (not represented in "{contractTypeRep}"s anymore) count: \
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

           synchronizeContracts nowDay handledContracts egSubPrograms


contractTypeRep :: TypeRep
contractTypeRep = typeRep (Proxy :: Proxy Contract)
