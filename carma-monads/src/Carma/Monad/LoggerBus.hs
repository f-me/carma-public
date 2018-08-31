{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Carma.Monad.LoggerBus
     ( genericLogImpl
     , readLogImpl
     , module Carma.Monad.LoggerBus.Class
     ) where

import qualified Data.Text as T
import           Text.InterpolatedString.QM

import           Control.Monad
import           Control.Concurrent (MVar)

import           Carma.Monad.MVar
import           Carma.Monad.Clock
import           Carma.Monad.Thread
import           Carma.Monad.LoggerBus.Types
import           Carma.Monad.LoggerBus.Class
import           Carma.Monad.LoggerBus.Helpers


-- | Generic implementation of log function from @MonadLoggerBus@
--
-- Could be used to implement @logDebug@, @logInfo@, @logWarn@ and @logError@.
genericLogImpl
  :: (MonadMVar m, MonadClock m, MonadThread m)
  => LogMessageType -> MVar LogMessage -> T.Text -> m ()
genericLogImpl logMsgType loggerBus' msg = do
  !utc <- getCurrentTime

  void $ fork $ -- Forking for non-blocking writing to @MVar@
    putMVar loggerBus' $ LogMessage logMsgType
      [qm| [{formatTime utc} UTC] {msg} |]


-- | Generic implementation of @readLog@ from @MonadLoggerBus@
readLogImpl :: MonadMVar m => MVar LogMessage -> m LogMessage
readLogImpl = takeMVar
