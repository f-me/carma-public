module Carma.Monad.LoggerBus.Types
     ( LogMessageType (..)
     , LogMessage (..)
     ) where

import qualified Data.Text as T


data LogMessageType = LogInfo | LogError deriving (Show, Eq)
data LogMessage     = LogMessage LogMessageType T.Text deriving (Show, Eq)
