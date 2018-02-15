module App.Store.Reducers
     ( AppState
     , appReducer
     , appInitialState
     ) where

import Prelude

import Data.Maybe (Maybe (..))

import Router (Location (Empty))
import App.Store.Actions (AppAction (..))

import App.Store.DiagTree.Reducers
     ( DiagTreeState
     , diagTreeInitialState
     , diagTreeReducer
     )


type AppState =
  { currentLocation :: Location
  , diagTree        :: DiagTreeState
  }

appInitialState :: AppState
appInitialState =
  { currentLocation : Empty
  , diagTree        : diagTreeInitialState
  }


appReducer :: AppState -> AppAction -> Maybe AppState

appReducer state (Navigate route) =
  if state.currentLocation /= route
     then Just $ state { currentLocation = route }
     else Nothing

appReducer state (DiagTree x) =
  diagTreeReducer state.diagTree x <#> state { diagTree = _ }
