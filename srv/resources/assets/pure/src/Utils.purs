module Utils
     ( module StoreConnect
     , module ReactComponent
     , (<.>)
     , addClassName
     , toMaybeT
     , eventInputValue
     , eventIsChecked
     , unfoldrBoundedEnum
     ) where

import Prelude

import Unsafe.Coerce (unsafeCoerce)
import Data.Tuple (Tuple (Tuple))
import Data.Maybe (Maybe (..))
import Data.Enum (class BoundedEnum, succ)
import Data.Unfoldable (class Unfoldable, unfoldr)

import Control.Monad.Maybe.Trans (MaybeT (MaybeT))

import React (Event)

import Utils.StoreConnect as StoreConnect
import Utils.ReactComponent as ReactComponent


addClassName :: String -> String -> String
addClassName a b = a <> " " <> b

infixr 5 addClassName as <.>


toMaybeT :: forall a m. Applicative m => Maybe a -> MaybeT m a
toMaybeT = MaybeT <<< pure


eventInputValue :: Event -> String
eventInputValue = unsafeCoerce >>> _.currentTarget.value

eventIsChecked :: Event -> Boolean
eventIsChecked = unsafeCoerce >>> _.currentTarget.checked


unfoldrBoundedEnum :: forall f a. Unfoldable f => BoundedEnum a => f a
unfoldrBoundedEnum = unfoldr (_ <#> \x -> Tuple x (succ x)) $ Just bottom
