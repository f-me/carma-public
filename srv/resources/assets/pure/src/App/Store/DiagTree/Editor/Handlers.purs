module App.Store.DiagTree.Editor.Handlers
     ( diagTreeEditorHandler
     ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)

import Data.JSDate (LOCALE)
import Data.Maybe (Maybe, fromMaybe)

import Network.HTTP.Affjax (AJAX)

import App.Store (AppContext)
import App.Store.DiagTree.Editor.Reducers (DiagTreeEditorState)
import App.Store.DiagTree.Editor.Actions (DiagTreeEditorAction (..))
import App.Store.DiagTree.Editor.Handlers.NewSlide (newSlide)
import App.Store.DiagTree.Editor.Handlers.LoadSlides (loadSlides)
import App.Store.DiagTree.Editor.Handlers.DeleteSlide (deleteSlide)
import App.Store.DiagTree.Editor.Handlers.SaveSlide (saveSlide)


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

diagTreeEditorHandler appCtx prevState nextState action = case action of

  LoadSlidesRequest ->
    if isProcessing
       then ignore
       else loadSlides appCtx

  NewSlideRequest ->
    if isProcessing
       then ignore
       else newSlide appCtx

  DeleteSlideRequest slidePath ->
    if isProcessing
       then ignore
       else let state = fromMaybe prevState nextState
             in deleteSlide appCtx state.slides slidePath

  SaveSlideRequest slidePath { slide, newAnswers } ->
    if isProcessing
       then ignore
       else saveSlide appCtx slidePath slide newAnswers

  _ -> ignore

  where
    ignore = pure unit

    isProcessing
       = prevState.isSlidesLoading
      || prevState.newSlide.isProcessing
      || prevState.slideDeleting.isProcessing
      || prevState.slideSaving.isProcessing
