module Utils
     ( module StoreConnect
     , module ReactComponent
     , (<.>)
     , addClassName
     , toMaybeT
     ) where

import Prelude

import Data.Maybe (Maybe)

import Control.Monad.Maybe.Trans (MaybeT (MaybeT))

import Utils.StoreConnect as StoreConnect
import Utils.ReactComponent as ReactComponent


addClassName :: String -> String -> String
addClassName a b = a <> " " <> b

infixr 5 addClassName as <.>


toMaybeT :: forall a m. Applicative m => Maybe a -> MaybeT m a
toMaybeT = MaybeT <<< pure
