{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, ViewPatterns #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | VIN synchronizer worker module.
--
-- Also known as integration point __CRM.EG.02__.
module Carma.EraGlonass.VinSynchronizer
     ( runVinSynchronizer
     ) where

import           Data.Function (fix)
import           Data.Time.Clock (utctDay)
import           Data.Time.LocalTime (TimeZone, utcToZonedTime)
import           Data.Time.Format
import           Data.Text (Text)
import           Text.InterpolatedString.QM
import           Text.Printf (printf)
import           Data.List (intersect)
import           Data.Maybe (catMaybes)

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Reader (MonadReader, ReaderT, asks)
import           Control.Monad.Catch
import           Control.Monad.Trans.Class (lift)
import           Control.Exception (SomeException, fromException)

import           Database.Persist ((==.), (!=.), (>=.), (<-.), (/<-.), (||.))
import           Database.Persist.Sql (SqlBackend)
import           Database.Persist.Types (entityVal)

import           Carma.Utils.Operators
import           Carma.Monad
import           Carma.Model.SubProgram.Persistent
import           Carma.Model.Contract.Persistent
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Instance.Persistent (TimeoutException (..))
import           Carma.EraGlonass.Types
import           Carma.EraGlonass.Model.EraGlonassSynchronizedContract.Persistent


-- | VIN synchronizer monad constraint.
type VinSynchronizerMonad m =
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadClock m
   , MonadDelay m
   , MonadPersistentSql m
   , MonadThrow m
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
synchronizeVins :: VinSynchronizerMonad m => ReaderT SqlBackend m ()
synchronizeVins = do
  logInfo [qn| Synchronizing VINs... |]

  -- Getting list of currently handled VINs to check if some of them
  -- should be marked for EG service as not handled by CaRMa anymore.
  handledContracts <-
    selectList [ EraGlonassSynchronizedContractIsHandledByCarma ==. True ] []

  -- Getting list of active EG participants @SubProgram@s.
  egSubPrograms <-
    selectKeysList [ SubProgramActive                ==. True
                   , SubProgramEraGlonassParticipant ==. True
                   ] []

  nowDay <- utctDay <$> lift getCurrentTime

  let handledVINs =
        handledContracts <&>
          Just . eraGlonassSynchronizedContractVin . entityVal

  -- Getting list of @Contract@s (VINs actually) which are used to be handled.
  -- Now we need to notify EG service that they aren't handled by CaRMa anymore.
  contractsToUnmarkAsHandled <- let

    selectFilter
      = -- @Contract@'s VIN is handled by CaRMa.
        ( ContractVin <-. handledVINs )

      : (   -- @Contract@ has been deactivated.
            [ ContractIsActive ==. False ]

        ||. -- @SubProgram@ of a @Contract@ has been unmarked as EG participant.
            [ ContractSubprogram /<-. (Just <$> egSubPrograms) ]

        ||. -- Validity date of a @Contract@ has been expired.
            [ ContractValidUntil !=. Nothing
            , ContractValidUntil >=. Just nowDay
            ]
        )

    in selectList selectFilter []

  -- Searching for incorrect data, when some VIN is handled by CaRMa but no
  -- longer represented in @Contract@s list.
  -- Usually it doesn't happen but if for instance you change @vin@ field value
  -- of a @Contract@ and previous @vin@ have been marked as handled by CaRMa for
  -- EG service we supposed to notify EG that we don't handle these anymore.
  ephemeralVINsToUnmarkAsHandled <-
    selectList [ ContractVin <-. handledVINs ] []
      <&> fmap (entityVal ? contractVin) ? catMaybes -- Extracting only VINs
      <&> \contractVINs ->
            let vinLens = entityVal ? eraGlonassSynchronizedContractVin

                f acc x =
                  if vinLens x `notElem` contractVINs
                     then x : acc
                     else acc

                in foldl f [] handledContracts

  -- Checking for data correctness.
  unless ( let vinsA =
                 catMaybes $
                   contractsToUnmarkAsHandled <&> entityVal ? contractVin

               vinsB =
                 ephemeralVINsToUnmarkAsHandled <&>
                   entityVal ? eraGlonassSynchronizedContractVin

               in null $ vinsA `intersect` vinsB ) $

    fail [qns|
      Ephemeral VINs and outdated/deactivated VINs cannot intersect,
      something wrong...
    |]

  logDebug [qn| testing... |]


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
