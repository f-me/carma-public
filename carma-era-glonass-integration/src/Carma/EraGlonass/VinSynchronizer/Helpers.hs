{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Helpers for VIN synchronizer.
module Carma.EraGlonass.VinSynchronizer.Helpers
     ( getTimeToWait
     ) where

import           Data.Time.LocalTime (TimeZone, utcToZonedTime)
import           Data.Time.Format

import           Control.Arrow

import           Carma.Utils.Operators
import           Carma.Monad.Clock


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
