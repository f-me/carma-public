module Carma.Monad.LoggerBus.Class
     ( MonadLoggerBus (..)
     ) where

import qualified Data.Text as T

import           Carma.Monad.LoggerBus.Types (LogMessage)


-- | Abstraction for logging.
--
-- Usually implemented using @MonadLogger@,
-- see @Carma.Monad.LoggerBus.MonadLogger@ for default implementation.
class Monad m => MonadLoggerBus m where
  logDebug :: T.Text -> m ()
  logInfo  :: T.Text -> m ()
  logWarn  :: T.Text -> m ()
  logError :: T.Text -> m ()
  readLog  :: m LogMessage
