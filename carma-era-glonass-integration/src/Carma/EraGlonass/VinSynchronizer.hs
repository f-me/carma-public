{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}
{-# LANGUAGE QuasiQuotes #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | VIN synchronizer worker module.
module Carma.EraGlonass.VinSynchronizer
     ( runVinSynchronizer
     ) where

import           Data.Time.LocalTime (TimeZone, utcToZonedTime)
import           Data.Time.Format
import           Text.InterpolatedString.QM
import           Text.Printf (printf)

import           Control.Arrow
import           Control.Monad.Reader (MonadReader)

import           Carma.Utils.Operators
import           Carma.Monad
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Types


-- | VIN synchronizer monad constraint.
type VinSynchronizerMonad m =
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadClock m
   )


-- | VIN synchronizer worker starter.
runVinSynchronizer :: VinSynchronizerMonad m => TimeZone -> m ()
runVinSynchronizer tz = do
  logInfo [qm| Running VIN synchronizer worker (timezone is {tz})... |]
  (hoursToWait, minutesToWait) <- getTimeToWait tz

  logDebug $ let zeroPad x = printf "%02d" (x :: Word) :: String in [qms|
    Waiting for {zeroPad $ floor hoursToWait}:{zeroPad minutesToWait}
    before 00:00 to trigger next VIN synchronization...
  |]

  -- TODO


{-|
Returns hours and minutes to wait before triggering next VIN synchronization.

Minutes already included to hours as fractional part.
-}
getTimeToWait :: MonadClock m => TimeZone -> m (Float, Word)
getTimeToWait tz = getCurrentTime <&> utcToZonedTime tz ? f
  where
    format = formatTime defaultTimeLocale
    readFloat = read :: String -> Float

    f =
      format "%H" &&& format "%M" -- Getting hours and minutes
      >>> readFloat *** readFloat
      >>> second (/ 60) -- Minutes to fractional part of an hour

      >>> -- Sum hours and fractional part of an hour (minutes),
          -- getting hours left to 00:00 (with fractional part).
          arr (uncurry (+) ? (`subtract` 24) ? (id &&& id))

      >>> -- Getting minutes left (as a remainder apart from hours).
          second ( (properFraction :: Float -> (Int, Float))
                 ? snd    -- Only remainder
                 ? (* 60) -- Back to real minutes
                 ? round
                 )
