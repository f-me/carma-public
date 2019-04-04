module App.Store.HandlersSpec
     ( subscribeHandlers
     ) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff_)

import App.Store (AppContext, subscribe')
import App.Store.Actions (AppAction (..))
import App.Store.Handlers (appHandler)
import App.Store.DiagTree.Actions (DiagTreeAction (..))
import App.Store.DiagTree.Editor.Handlers (diagTreeEditorHandler)


subscribeHandlers :: AppContext -> Aff Unit
subscribeHandlers appCtx = go where
  go = do

    runHandler \ { prevState, nextState, action } -> case action of
      Navigate _ -> app prevState nextState action
      _ -> ignore

    runHandler \ { prevState, nextState, action } -> case action of
      DiagTree (Editor x) ->
        diagTreeEditor (diagTreeEditorLens prevState)
                       (diagTreeEditorLens <$> nextState)
                       x
      _ -> ignore

  ignore = pure unit
  app = appHandler appCtx

  diagTreeEditor = diagTreeEditorHandler appCtx
  diagTreeEditorLens = _.diagTree.editor

  runHandler selector = void $ liftEffect m where
    m = subscribe' appCtx $ launchAff_ <<< selector
