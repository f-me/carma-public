module Main (_main) where

import Prelude
import Effect.Unsafe (unsafePerformEffect)

-- local imports
import ApplicationInit (runApplication)


_main :: Unit
_main = unsafePerformEffect runApplication
