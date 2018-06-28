{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Carma.Monad.LoggerBus
     ( logInfoImpl
     , logErrorImpl
     , readLogImpl
     , module Carma.Monad.LoggerBus.Class
     ) where

import qualified Data.Text as T
import qualified Data.Time.Format as Time
import           Text.InterpolatedString.QM

import           Control.Monad
import           Control.Concurrent (MVar)

import           Carma.Monad.MVar
import           Carma.Monad.Clock
import           Carma.Monad.Thread
import           Carma.Monad.LoggerBus.Types
import           Carma.Monad.LoggerBus.Class


-- Generic implementation of `logInfo` from `MonadLoggerBus`
logInfoImpl
  :: (MonadMVar m, MonadClock m, MonadThread m)
  => MVar LogMessage -> T.Text -> m ()
logInfoImpl loggerBus' msg = do
  !utc <- getCurrentTime

  void $ fork $ -- Forking for non-blocking writing to MVar
    putMVar loggerBus' $ LogMessage LogInfo
      [qm| [{formatTime utc} UTC] {msg} |]


-- Generic implementation of `logError` from `MonadLoggerBus`
logErrorImpl
  :: (MonadMVar m, MonadClock m, MonadThread m)
  => MVar LogMessage -> T.Text -> m ()
logErrorImpl loggerBus' msg = do
  !utc <- getCurrentTime

  void $ fork $ -- Forking for non-blocking writing to MVar
    putMVar loggerBus' $ LogMessage LogError
      [qm| [{formatTime utc} UTC] {msg} |]


-- Generic implementation of `readLog` from `MonadLoggerBus`
readLogImpl :: MonadMVar m => MVar LogMessage -> m LogMessage
readLogImpl = takeMVar


formatTime :: Time.FormatTime t => t -> String
formatTime = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S"
