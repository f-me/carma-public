-- This module has some miscellaneous functions/values used in this service.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Carma.NominatimMediator.Utils
     ( secondInMicroseconds
     , floatShow
     , unwrapperToProxy
     ) where

import           Data.Proxy
import           Text.Printf (printf)


secondInMicroseconds :: Float
secondInMicroseconds = 1000 * 1000

floatShow :: Float -> String
floatShow = printf "%f"

unwrapperToProxy :: (a -> b) -> Proxy b
unwrapperToProxy _ = Proxy
