module Carma.Monad.LoggerBus.Class
     ( MonadLoggerBus (..)
     ) where

import qualified Data.Text as T

import           Carma.Monad.LoggerBus.Types (LogMessage)


class Monad m => MonadLoggerBus m where
  logInfo  :: T.Text -> m ()
  logError :: T.Text -> m ()
  readLog  :: m LogMessage
