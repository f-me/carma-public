module Carma.Monad.LoggerBus.Helpers
     ( formatTime
     ) where

import qualified Data.Time.Format as Time


formatTime :: Time.FormatTime t => t -> String
formatTime = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S"
