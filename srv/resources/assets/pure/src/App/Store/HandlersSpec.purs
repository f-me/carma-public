module App.Store.HandlersSpec
     ( subscribeHandlers
     ) where

import Prelude

import Control.Monad.Rec.Class (forever)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Aff.AVar (AVAR, takeVar)

import Network.HTTP.Affjax (AJAX)
import DOM (DOM)

import App.Store (AppContext, subscribe', getSubscriberBus)
import App.Store.Actions (AppAction (..))
import App.Store.Handlers (appHandler)
import App.Store.DiagTree.Actions (DiagTreeAction (..))
import App.Store.DiagTree.Editor.Handlers (diagTreeEditorHandler)


subscribeHandlers
  :: forall eff
   . AppContext
  -> Aff ( ref     :: REF
         , avar    :: AVAR
         , dom     :: DOM
         , console :: CONSOLE
         , ajax    :: AJAX
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
