module App.Store.Handlers
     ( appHandler
     ) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Aff (Aff)

import Data.Maybe (Maybe)

import Router (navigateToRoute)
import App.Store (Store)
import App.Store.Actions (AppAction (..))
import App.Store.Reducers (AppState)


appHandler
  :: forall state action
   . Store state action
  -> AppState
  -> Maybe AppState
  -> AppAction
  -> Aff Unit

appHandler _ _ _ = case _ of
  Navigate route -> liftEffect $ navigateToRoute route
  _ -> pure unit
