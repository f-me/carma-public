module App.Store.DiagTree.Editor.Handlers
     ( diagTreeEditorHandler
     ) where

import Prelude

import Data.Maybe (Maybe, fromMaybe)

import Effect.Aff (Aff)

import App.Store (Store)
import App.Store.Actions (AppAction)
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
  :: forall state
   . Store state AppAction
  -> DiagTreeEditorState
  -> Maybe DiagTreeEditorState
  -> DiagTreeEditorAction
  -> Aff Unit

diagTreeEditorHandler store prevState nextState = case _ of

  LoadSlidesRequest ->
    if isProcessing
       then ignore
       else loadSlides store

  NewSlideRequest ->
    if isProcessing
       then ignore
       else newSlide store

  DeleteSlideRequest slidePath ->
    if isProcessing
       then ignore
       else let state = fromMaybe prevState nextState
             in deleteSlide store state.slides slidePath

  CopySlideRequest slidePath ->
    if isProcessing
       then ignore
       else let state = fromMaybe prevState nextState
             in copySlide store state

  CutSlideRequest slidePath ->
    if isProcessing
       then ignore
       else let state = fromMaybe prevState nextState
             in cutSlide store state

  PasteSlideRequest slidePath ->
    if isProcessing
       then ignore
       else let state = fromMaybe prevState nextState
             in pasteSlide store prevState slidePath

  SaveSlideRequest slidePath { slide, newAnswers } ->
    if isProcessing
       then ignore
       else saveSlide store slidePath slide newAnswers

  _ -> ignore

  where
    ignore = pure unit

    isProcessing
       = prevState.isSlidesLoading
      || prevState.newSlide.isProcessing
      || prevState.slideDeleting.isProcessing
      || prevState.copyPasteBuffer.isProcessing
      || prevState.slideSaving.isProcessing
