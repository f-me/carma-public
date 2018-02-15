module App.Store.DiagTree.Editor.Handlers
     ( diagTreeEditorHandler
     ) where

import Prelude

import Control.Monad.Eff.Console (log)
import Control.Monad.Aff (Aff, liftEff')

import App.Store (AppContext)
import App.Store.Types (AppContextEffects, StoreEffects)
import App.Store.DiagTree.Editor.Actions (DiagTreeEditorAction (..))
import App.Store.DiagTree.Editor.Reducers (DiagTreeEditorState)


diagTreeEditorHandler
  :: forall eff
   . AppContext (AppContextEffects eff)
  -> DiagTreeEditorState
  -> DiagTreeEditorAction
  -> Aff (StoreEffects eff) Unit

diagTreeEditorHandler appCtx state action = case action of
  LoadSlidesRequest   -> liftEff' $ log "request"
  LoadSlidesSuccess _ -> liftEff' $ log "success"
  LoadSlidesFailure _ -> liftEff' $ log "failure"
