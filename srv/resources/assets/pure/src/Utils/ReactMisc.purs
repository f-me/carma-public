module Utils.ReactMisc
     ( callEventHandler
     , eventInputValue
     , eventIsChecked
     ) where

import Prelude
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Eff (Eff)
import React (Event, EventHandler)


foreign import callEventHandler
  :: forall a eff . EventHandler a -> a -> Eff eff Unit


eventInputValue :: Event -> String
eventInputValue = unsafeCoerce >>> _.currentTarget.value

eventIsChecked :: Event -> Boolean
eventIsChecked = unsafeCoerce >>> _.currentTarget.checked
