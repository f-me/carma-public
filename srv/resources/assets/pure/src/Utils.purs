module Utils
     ( module StoreConnect
     , module ReactComponent
     , module ReactMisc
     , module ShowCase
     , module Sex
     , (<.>)
     , addClassName
     , toMaybeT
     , unfoldrBoundedEnum
     , capitalize
     ) where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(MaybeT))
import Data.Enum (class BoundedEnum, succ)
import Data.Maybe (Maybe(..), maybe)
import Data.String (uncons, toUpper, singleton)
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Utils.ReactComponent as ReactComponent
import Utils.ReactMisc as ReactMisc
import Utils.Sex as Sex
import Utils.ShowCase as ShowCase
import Utils.StoreConnect as StoreConnect


addClassName :: String -> String -> String
addClassName a b = a <> " " <> b

infixr 5 addClassName as <.>


toMaybeT :: forall a m. Applicative m => Maybe a -> MaybeT m a
toMaybeT = MaybeT <<< pure


unfoldrBoundedEnum :: forall f a. Unfoldable f => BoundedEnum a => f a
unfoldrBoundedEnum = unfoldr (_ <#> \x -> Tuple x (succ x)) $ Just bottom


capitalize :: String -> String
capitalize = uncons >>> maybe "" f
  where f { head, tail } = toUpper (singleton head) <> tail
