module Carma.EraGlonass.Types
     ( AppContext (..)
     ) where

import           Control.Concurrent.MVar (MVar)

import           Carma.Monad.LoggerBus.Types (LogMessage)


data AppContext
   = AppContext
   { -- A bus to send log messages to
     loggerBus :: MVar LogMessage
   }
