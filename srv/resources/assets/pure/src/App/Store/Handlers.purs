module App.Store.Handlers
     ( appHandler
     ) where

import Prelude

import Control.Monad.Aff (Aff, liftEff')

import Router (navigateToRoute)
import App.Store (AppContext)
import App.Store.Types (AppContextEffects, StoreEffects)
import App.Store.Actions (AppAction (..))
import App.Store.Reducers (AppState)
import App.Store.DiagTree.Handlers (diagTreeHandler)


appHandler
  :: forall eff
   . AppContext (AppContextEffects eff)
  -> AppState
  -> AppAction
  -> Aff (StoreEffects eff) Unit

appHandler appCtx state action = case action of
  Navigate route -> liftEff' $ navigateToRoute route
  DiagTree x     -> diagTree state.diagTree x

  where
    diagTree = diagTreeHandler appCtx
