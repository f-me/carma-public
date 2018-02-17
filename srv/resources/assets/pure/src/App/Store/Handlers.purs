module App.Store.Handlers
     ( appHandler
     , subscribeHandlers
     ) where

import Prelude

import Control.Monad.Rec.Class (forever)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Aff.AVar (AVAR, takeVar)

import DOM (DOM)

import Router (navigateToRoute)
import App.Store (AppContext, subscribe', getSubscriberBus)
import App.Store.Actions (AppAction (..))
import App.Store.Reducers (AppState)
import App.Store.DiagTree.Actions (DiagTreeAction (..))
import App.Store.DiagTree.Editor.Handlers (diagTreeEditorHandler)


appHandler
  :: forall eff
   . AppContext
  -> AppState
  -> AppAction
  -> Aff (dom :: DOM | eff) Unit

appHandler appCtx state action = case action of
  Navigate route -> liftEff $ navigateToRoute route
  _ -> pure unit


subscribeHandlers
  :: forall eff
   . AppContext
  -> Aff ( ref     :: REF
         , avar    :: AVAR
         , dom     :: DOM
         , console :: CONSOLE
         | eff
         ) Unit

subscribeHandlers appCtx = do

  runHandler $ \ev -> case ev.action of
    Navigate _ -> app ev.state ev.action
    _ -> ignore

  runHandler $ \ev -> case ev.action of
    DiagTree (Editor x) -> diagTreeEditor ev.state.diagTree.editor x
    _ -> ignore

  where
    ignore = pure unit
    app = appHandler appCtx
    diagTreeEditor = diagTreeEditorHandler appCtx

    runHandler selector = do
      handlerSubscription <- liftEff $ subscribe' appCtx
      handlerBus <- getSubscriberBus appCtx handlerSubscription
      void $ forkAff $ forever $ takeVar handlerBus >>= selector
