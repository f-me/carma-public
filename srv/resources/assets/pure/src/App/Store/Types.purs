module App.Store.Types
     ( StoreReducer
     , StoreSubscriber
     ) where

import Prelude

import Data.Maybe (Maybe)

import Control.Monad.Eff.Ref (REF)
import Control.Monad.Aff (Aff)

import App.Store.Actions (AppAction)
import App.Store.Reducers (AppState)


-- `Maybe` here to be able to avoid notifying subscribers
-- (when state isn't changed for example).
type StoreReducer =
  AppState -> AppAction -> Maybe AppState

type StoreSubscriber eff =
  AppState -> AppAction -> Aff (ref :: REF | eff) Unit
