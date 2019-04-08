module Utils
     ( module StoreConnect
     , module ShowCase
     , module Sex
     , (<.>)
     , addClassName
     , toMaybeT
     , unfoldrBoundedEnum
     , capitalize
     , eventIsChecked
     , eventInputValue
     ) where

import Prelude
import Unsafe.Coerce (unsafeCoerce)

import Data.Enum (class BoundedEnum, succ)
import Data.Maybe (Maybe (..), maybe)
import Data.String (uncons, toUpper, singleton)
import Data.Tuple (Tuple (Tuple))
import Data.Unfoldable (class Unfoldable, unfoldr)

import Control.Monad.Maybe.Trans (MaybeT (MaybeT))

import Effect (Effect)

import React.SyntheticEvent (SyntheticInputEvent, currentTarget)

import Utils.Sex as Sex
import Utils.ShowCase as ShowCase
import Utils.StoreConnect as StoreConnect


addClassName :: String -> String -> String
addClassName a b = a <> " " <> b

-- Use this operator only to combine class names.
-- DO NOT use it for separation with spaces.
infixr 5 addClassName as <.>


toMaybeT :: forall a m. Applicative m => Maybe a -> MaybeT m a
toMaybeT = MaybeT <<< pure


unfoldrBoundedEnum :: forall f a. Unfoldable f => BoundedEnum a => f a
unfoldrBoundedEnum = unfoldr (_ <#> \x -> Tuple x $ succ x) $ Just bottom


capitalize :: String -> String
capitalize = uncons >>> maybe "" f
  where f { head, tail } = toUpper (singleton head) <> tail


eventIsChecked :: SyntheticInputEvent -> Effect Boolean
eventIsChecked event = currentTarget event <#> unsafeCoerce >>> _.checked

eventInputValue :: SyntheticInputEvent -> Effect String
eventInputValue event = currentTarget event <#> unsafeCoerce >>> _.value
