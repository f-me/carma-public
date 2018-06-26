-- This module has some miscellaneous functions/values used in this service.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Carma.NominatimMediator.Utils
     ( secondInMicroseconds
     , timeFormat
     , formatTime
     , floatShow
     , unwrapperToProxy
     ) where

import           Data.Proxy
import qualified Data.Time.Format as Time
import           Text.Printf (printf)


secondInMicroseconds :: Float
secondInMicroseconds = 1000 * 1000

timeFormat :: String
timeFormat = "%Y-%m-%d %H:%M:%S"

formatTime :: Time.FormatTime t => t -> String
formatTime = Time.formatTime Time.defaultTimeLocale timeFormat

floatShow :: Float -> String
floatShow = printf "%f"

unwrapperToProxy :: (a -> b) -> Proxy b
unwrapperToProxy _ = Proxy
