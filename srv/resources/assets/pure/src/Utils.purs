module Utils
     ( module StoreConnect
     , module ReactComponent
     , module ShowCase
     , module Sex
     , (<.>)
     , addClassName
     , toMaybeT
     , eventInputValue
     , eventIsChecked
     , unfoldrBoundedEnum
     , capitalize
     ) where

import Prelude

import Unsafe.Coerce (unsafeCoerce)
import Data.Tuple (Tuple (Tuple))
import Data.Maybe (Maybe (..), maybe)
import Data.Enum (class BoundedEnum, succ)
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.String (uncons, toUpper, singleton)

import Control.Monad.Maybe.Trans (MaybeT (MaybeT))

import React (Event)

import Utils.StoreConnect as StoreConnect
import Utils.ReactComponent as ReactComponent
import Utils.ShowCase as ShowCase
import Utils.Sex as Sex


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


capitalize :: String -> String
capitalize = uncons >>> maybe "" f
  where f { head, tail } = toUpper (singleton head) <> tail
