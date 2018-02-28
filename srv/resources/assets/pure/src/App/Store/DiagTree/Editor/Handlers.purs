module App.Store.DiagTree.Editor.Handlers
     ( diagTreeEditorHandler
     ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)

import Data.JSDate (LOCALE)

import Network.HTTP.Affjax (AJAX)

import App.Store (AppContext)
import App.Store.DiagTree.Editor.Reducers (DiagTreeEditorState)
import App.Store.DiagTree.Editor.Actions (DiagTreeEditorAction (..))
import App.Store.DiagTree.Editor.Handlers.NewSlide (newSlide)
import App.Store.DiagTree.Editor.Handlers.LoadSlides (loadSlides)
import App.Store.DiagTree.Editor.Handlers.DeleteSlide (deleteSlide)


diagTreeEditorHandler
  :: forall eff
   . AppContext
  -> DiagTreeEditorState
  -> DiagTreeEditorAction
  -> Aff ( ajax    :: AJAX
         , avar    :: AVAR
         , locale  :: LOCALE
         , console :: CONSOLE
         | eff
         ) Unit

diagTreeEditorHandler appCtx state action = case action of
  NewSlideRequest              -> newSlide    appCtx
  LoadSlidesRequest            -> loadSlides  appCtx
  DeleteSlideRequest slidePath -> deleteSlide appCtx slidePath
  _ -> pure unit
