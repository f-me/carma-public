module App.Store.DiagTree.Editor.Handlers.Helpers
     ( errLog
     , sendAction
     ) where

import Prelude

import Effect.Aff (Aff)
import Effect.Console (error)
import Effect.Class (liftEffect)

import App.Store (AppContext, dispatch)
import App.Store.Actions (AppAction (DiagTree))
import App.Store.DiagTree.Actions (DiagTreeAction (Editor))
import App.Store.DiagTree.Editor.Actions (DiagTreeEditorAction)


errLog :: String -> Aff Unit
errLog = liftEffect <<< error <<< ("Diag Tree Editor: " <> _)


sendAction :: AppContext -> DiagTreeEditorAction -> Aff Unit
sendAction appCtx = dispatch appCtx <<< DiagTree <<< Editor
