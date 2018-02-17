module App.Store.DiagTree.Handlers
     ( diagTreeHandler
     ) where

import Prelude

import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff (Aff)

import App.Store (AppContext)
import App.Store.DiagTree.Actions (DiagTreeAction (..))
import App.Store.DiagTree.Reducers (DiagTreeState)
import App.Store.DiagTree.Editor.Handlers (diagTreeEditorHandler)


diagTreeHandler
  :: forall eff
   . AppContext
  -> DiagTreeState
  -> DiagTreeAction
  -> Aff (console :: CONSOLE | eff) Unit

diagTreeHandler appCtx state action = case action of
  Editor x -> editor state.editor x

  where
    editor = diagTreeEditorHandler appCtx
