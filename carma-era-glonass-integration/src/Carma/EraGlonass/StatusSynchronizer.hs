{-# LANGUAGE OverloadedStrings, QuasiQuotes, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Request for service status synchronizer worker module.
module Carma.EraGlonass.StatusSynchronizer
     ( runStatusSynchronizer
     ) where

import           Data.Text (Text)
import           Text.InterpolatedString.QM
import           Text.Printf (printf)

import           Control.Monad
import           Control.Arrow ((&&&), (***), (>>>))
import           Control.Monad.Reader (MonadReader, ReaderT, asks)
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar

import           Database.Persist.Sql (SqlBackend)

import           Carma.Monad
import           Carma.Utils
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Types.AppContext (AppContext (..))
import           Carma.EraGlonass.Types.Helpers.DateTime (showRFC3339DateTime)
import           Carma.EraGlonass.StatusSynchronizer.Helpers


runStatusSynchronizer
  :: forall m
   .
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadDelay m -- To wait before next synchronization.
   , MonadThread m
   , MonadSTM m
   , MonadPersistentSql m
   )
  => m ()

runStatusSynchronizer = do
  srcLogInfo "Running Status Synchronizer worker..."
  srcLogWarn "TODO Implement Status Synchronizer!"

  forever $ do
    manualTriggerBus <- asks statusSynchronizerTriggerBus
    bgThreadsCounter <- asks backgroundTasksCounter
    interval         <- asks statusSynchronizerInterval

    (wait, done) <- atomically $ do
      _ <- tryTakeTMVar manualTriggerBus -- Flusing previous state
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
      flip forkFinally (const onForkDeath) $ do
        time <- atomically $ readTMVar manualTriggerBus
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

synchronizeStatuses
  :: forall m
   .
   ( MonadLoggerBus m
   )
  => ReaderT SqlBackend m ()

synchronizeStatuses =
  srcLogWarn "TODO Implement Status Synchronizer synchronization itereation!"
