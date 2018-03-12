module Utils.React.OutsideClick
     ( OutsideClick
     , OutsideClickSubscription
     , subscribeOutsideClick
     , unsubscribeOutsideClick
     ) where

import Prelude
import Control.Monad.Eff (kind Effect, Eff)
import React (Ref)


foreign import data OutsideClick :: Effect
foreign import data OutsideClickSubscription :: Type

foreign import subscribeOutsideClick
  :: forall eff
   . Eff (outsideClick :: OutsideClick | eff) Unit
  -> Ref
  -> Eff (outsideClick :: OutsideClick | eff) OutsideClickSubscription

foreign import unsubscribeOutsideClick
  :: forall eff
   . OutsideClickSubscription
  -> Eff (outsideClick :: OutsideClick | eff) Unit
