module App.Store.Handlers
     ( appHandler
     ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)

import DOM (DOM)

import Router (navigateToRoute)
import App.Store (AppContext)
import App.Store.Actions (AppAction (..))
import App.Store.Reducers (AppState)
import App.Store.DiagTree.Handlers (diagTreeHandler)


appHandler
  :: forall eff
   . AppContext
  -> AppState
  -> AppAction
  -> Aff (console :: CONSOLE, dom :: DOM | eff) Unit

appHandler appCtx state action = case action of
  Navigate route -> liftEff $ navigateToRoute route
  DiagTree x     -> diagTree state.diagTree x

  where
    diagTree = diagTreeHandler appCtx
