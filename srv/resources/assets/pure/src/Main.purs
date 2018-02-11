module Main (_main) where

import Prelude
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import App (runApp)

_main :: Unit
_main = unsafePerformEff runApp
