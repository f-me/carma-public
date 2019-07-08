module Carma.Monad.LoggerBus.Class
     ( MonadLoggerBus (..)
     ) where

import qualified Data.Text as T

import           Control.Monad.Logger (LogSource)

import           Carma.Monad.LoggerBus.Types (LogMessage)


-- | Abstraction for logging.
--
-- Usually implemented using @MonadLogger@,
-- see @Carma.Monad.LoggerBus.MonadLogger@ for default implementation.
class Monad m => MonadLoggerBus m where
  logDebug  :: T.Text -> m ()
  logDebugS :: LogSource -> T.Text -> m ()

  logInfo   :: T.Text -> m ()
  logInfoS  :: LogSource -> T.Text -> m ()

  logWarn   :: T.Text -> m ()
  logWarnS  :: LogSource -> T.Text -> m ()

  logError  :: T.Text -> m ()
  logErrorS :: LogSource -> T.Text -> m ()

  readLog   :: m LogMessage
