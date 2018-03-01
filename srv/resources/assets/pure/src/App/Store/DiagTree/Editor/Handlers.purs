module App.Store.DiagTree.Editor.Handlers
     ( diagTreeEditorHandler
     ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)

import Data.JSDate (LOCALE)
import Data.Maybe (Maybe)

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
  -> Maybe DiagTreeEditorState
  -> DiagTreeEditorAction
  -> Aff ( ajax    :: AJAX
         , avar    :: AVAR
         , locale  :: LOCALE
         , console :: CONSOLE
         | eff
         ) Unit

diagTreeEditorHandler appCtx prevState _ action = case action of

  LoadSlidesRequest ->
    if prevState.isSlidesLoading
       then ignore
       else loadSlides appCtx

  NewSlideRequest ->
    if prevState.newSlide.isProcessing
       then ignore
       else newSlide appCtx

  DeleteSlideRequest slidePath ->
    if prevState.slideDeleting.isProcessing
       then ignore
       else deleteSlide appCtx slidePath

  _ -> ignore

  where
    ignore = pure unit
