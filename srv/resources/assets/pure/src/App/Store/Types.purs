module App.Store.Types
     ( AppState
     , AppAction (..)
     , StoreReducer
     , StoreSubscriber
     , appInitialState
     ) where

import Prelude

import Data.Maybe (Maybe)

import Control.Monad.Eff.Ref (REF)
import Control.Monad.Aff (Aff)

import Router (Location (Empty))

import App.Store.DiagTree ( DiagTreeState
                          , DiagTreeAction
                          , diagTreeInitialState
                          )


type AppState =
  { currentLocation :: Location
  , diagTree        :: DiagTreeState
  }


data AppAction
  = Navigate Location
  | DiagTree DiagTreeAction


-- `Maybe` here to be able to avoid notifying subscribers
-- (when state isn't changed for example).
type StoreReducer =
  AppState -> AppAction -> Maybe AppState

type StoreSubscriber eff =
  AppState -> AppAction -> Aff (ref :: REF | eff) Unit


appInitialState :: AppState
appInitialState =
  { currentLocation : Empty
  , diagTree        : diagTreeInitialState
  }
