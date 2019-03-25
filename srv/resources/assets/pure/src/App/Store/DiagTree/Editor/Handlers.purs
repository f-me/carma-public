module App.Store.DiagTree.Editor.Handlers
     ( diagTreeEditorHandler
     ) where

import Prelude

import Data.Maybe (Maybe, fromMaybe)

import Effect.Aff (Aff)

import App.Store (AppContext)
import App.Store.DiagTree.Editor.Actions (DiagTreeEditorAction (..))
import App.Store.DiagTree.Editor.Handlers.CopySlide (copySlide)
import App.Store.DiagTree.Editor.Handlers.CutSlide (cutSlide)
import App.Store.DiagTree.Editor.Handlers.PasteSlide (pasteSlide)
import App.Store.DiagTree.Editor.Handlers.DeleteSlide (deleteSlide)
import App.Store.DiagTree.Editor.Handlers.LoadSlides (loadSlides)
import App.Store.DiagTree.Editor.Handlers.NewSlide (newSlide)
import App.Store.DiagTree.Editor.Handlers.SaveSlide (saveSlide)
import App.Store.DiagTree.Editor.Reducers (DiagTreeEditorState)


diagTreeEditorHandler
  :: AppContext
  -> DiagTreeEditorState
  -> Maybe DiagTreeEditorState
  -> DiagTreeEditorAction
  -> Aff Unit

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

  CopySlideRequest slidePath ->
    if isProcessing
       then ignore
       else let state = fromMaybe prevState nextState
             in copySlide appCtx state

  CutSlideRequest slidePath ->
    if isProcessing
       then ignore
       else let state = fromMaybe prevState nextState
             in cutSlide appCtx state

  PasteSlideRequest slidePath ->
    if isProcessing
       then ignore
       else let state = fromMaybe prevState nextState
             in pasteSlide appCtx prevState slidePath

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
      || prevState.copyPasteBuffer.isProcessing
      || prevState.slideSaving.isProcessing
