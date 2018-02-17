module App.Store.DiagTree.Editor.Handlers
     ( diagTreeEditorHandler
     ) where

import Prelude

import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff)

import App.Store (AppContext)
import App.Store.DiagTree.Editor.Actions (DiagTreeEditorAction (..))
import App.Store.DiagTree.Editor.Reducers (DiagTreeEditorState)


diagTreeEditorHandler
  :: forall eff
   . AppContext
  -> DiagTreeEditorState
  -> DiagTreeEditorAction
  -> Aff (console :: CONSOLE | eff) Unit

diagTreeEditorHandler appCtx state action = case action of
  LoadSlidesRequest   -> liftEff $ log "request"
  LoadSlidesSuccess _ -> liftEff $ log "success"
  LoadSlidesFailure _ -> liftEff $ log "failure"
