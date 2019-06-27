{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes, ViewPatterns, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances, FlexibleContexts #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | VIN synchronizer worker module.
module Carma.EraGlonass.VinSynchronizer
     ( runVinSynchronizer
     ) where

import           Data.Function (fix)
import           Data.Time.LocalTime (TimeZone)
import           Data.Text (Text)
import           Text.InterpolatedString.QM
import           Text.Printf (printf)

import           Control.Monad
import           Control.Monad.Reader (MonadReader, ReaderT, asks)
import           Control.Monad.Catch

import           Database.Persist.Sql (SqlBackend)

import           Carma.Monad
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Instance.Persistent (TimeoutException (..))
import           Carma.EraGlonass.Types.AppContext (AppContext (..))
import           Carma.EraGlonass.VinSynchronizer.Types
import           Carma.EraGlonass.VinSynchronizer.Helpers


-- | VIN synchronizer worker starter.
runVinSynchronizer :: VinSynchronizerMonad m => TimeZone -> m ()
runVinSynchronizer tz = go where
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
    logInfo [qm| Running VIN synchronizer worker (timezone is {tz})... |]

    forever $ do
      (hoursToWait, minutesToWait) <- getTimeToWait tz

      logDebug $ let zeroPad x = printf "%02d" (x :: Word) :: String in [qms|
        Waiting for {zeroPad $ floor hoursToWait}:{zeroPad minutesToWait}
        before 00:00 to trigger next VIN synchronization...
      |]

      -- delay $ round $ hoursToWait * 3600 * (10 ** 6) -- TODO uncomment
      delay $ 3 * (10 ^ (6 :: Int)) -- TODO remove (for testing purposes), 3 secs
      logDebug [qns| It's about 00:00,
                     initiating VIN synchronization process... |]

      fix $ \again ->
        asks vinSynchronizerTimeout
          >>= flip runSqlTimeout synchronizeVins
          >>= synchronizationResolve
          >>= flip unless again

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
synchronizeVins :: VinSynchronizerMonad m => ReaderT SqlBackend m ()
synchronizeVins = logError [qn| TODO Implement "Synchronizing VINs"! |]
