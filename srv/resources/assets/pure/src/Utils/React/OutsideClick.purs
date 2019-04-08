module Utils.React.OutsideClick
     ( OutsideClickSubscription
     , subscribeOutsideClick
     , unsubscribeOutsideClick
     ) where

import Prelude
import Effect (Effect)
import React (ReactRef)


foreign import data OutsideClickSubscription :: Type

foreign import subscribeOutsideClick
  :: Effect Unit -> ReactRef -> Effect OutsideClickSubscription

foreign import unsubscribeOutsideClick
  :: OutsideClickSubscription -> Effect Unit
