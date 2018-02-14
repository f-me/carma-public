module Main (_main) where

import Prelude
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import ApplicationInit (runApplication)

_main :: Unit
_main = unsafePerformEff runApplication
