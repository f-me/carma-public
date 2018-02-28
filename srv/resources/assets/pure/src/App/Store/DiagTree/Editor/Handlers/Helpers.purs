module App.Store.DiagTree.Editor.Handlers.Helpers
     ( errLog
     , sendAction
     ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE, error)
import Control.Monad.Eff.Class (liftEff)

import App.Store (AppContext, dispatch)
import App.Store.Actions (AppAction (DiagTree))
import App.Store.DiagTree.Actions (DiagTreeAction (Editor))
import App.Store.DiagTree.Editor.Actions (DiagTreeEditorAction)


errLog :: forall eff. String -> Aff (console :: CONSOLE | eff) Unit
errLog = liftEff <<< error <<< ("Diag Tree Editor: " <> _)


sendAction :: forall eff.
  AppContext -> DiagTreeEditorAction -> Aff (avar :: AVAR | eff) Unit

sendAction appCtx = dispatch appCtx <<< DiagTree <<< Editor
