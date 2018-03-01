module App.Store.Handlers
     ( appHandler
     ) where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff)

import Data.Maybe (Maybe)

import DOM (DOM)

import Router (navigateToRoute)
import App.Store (AppContext)
import App.Store.Actions (AppAction (..))
import App.Store.Reducers (AppState)


appHandler
  :: forall eff
   . AppContext
  -> AppState
  -> Maybe AppState
  -> AppAction
  -> Aff (dom :: DOM | eff) Unit

appHandler appCtx _ _ action = case action of
  Navigate route -> liftEff $ navigateToRoute route
  _ -> pure unit
