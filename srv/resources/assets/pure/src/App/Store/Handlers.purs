module App.Store.Handlers
     ( appHandler
     ) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Aff (Aff)

import Data.Maybe (Maybe)

import Router (navigateToRoute)
import App.Store (AppContext)
import App.Store.Actions (AppAction (..))
import App.Store.Reducers (AppState)


appHandler :: AppContext -> AppState -> Maybe AppState -> AppAction -> Aff Unit
appHandler appCtx _ _ action = case action of
  Navigate route -> liftEffect $ navigateToRoute route
  _ -> pure unit
