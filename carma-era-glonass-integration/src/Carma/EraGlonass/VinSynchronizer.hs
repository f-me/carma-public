{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase, ViewPatterns #-}
{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}
{-# LANGUAGE QuasiQuotes #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | VIN synchronizer worker module.
--
-- Also known as integration point __CRM.EG.02__.
module Carma.EraGlonass.VinSynchronizer
     ( runVinSynchronizer
     ) where

import           Data.Function (fix)
import           Data.Time.LocalTime (TimeZone, utcToZonedTime)
import           Data.Time.Format
import           Data.Text (Text)
import           Text.InterpolatedString.QM
import           Text.Printf (printf)

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Reader (MonadReader, ReaderT, asks)
import           Control.Monad.Catch
import           Control.Exception (fromException)

import           Database.Persist.Sql (SqlBackend)

import           Carma.Utils.Operators
import           Carma.Monad
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Instance.Persistent (TimeoutException (..))
import           Carma.EraGlonass.Types


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
    retryInterval <- asks vinSynchronizerRetryInterval

    fix $ \again ->
      asks vinSynchronizerTimeout
        >>= flip runSqlTimeout synchronizeVins
        >>= \case Right _ ->
                    logInfo [qn|
                      VINs synchronization iteration is finished successfully.
                    |]

                  Left (fromException -> Just (TimeoutExceeded n)) -> do
                    logError [qms|
                      Synchronization of VINs is failed becuase it is exceeded
                        timeout of
                        {round $ (fromIntegral n / 1000 / 1000 :: Float) :: Int}
                        second(s).
                      {willBeRetried retryInterval}
                    |]

                    delay retryInterval
                    logInfo $ retrying retryInterval
                    again

                  Left exception -> do
                    logError [qms|
                      Synchronization of VINs is failed with exception:
                        {exception}.
                      {willBeRetried retryInterval}
                    |]

                    delay retryInterval
                    logInfo $ retrying retryInterval
                    again

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


-- | VINs synchronization logic handler.
synchronizeVins :: VinSynchronizerMonad m => ReaderT SqlBackend m ()
synchronizeVins = do
  logInfo [qn| Synchronizing VINs... |]


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
