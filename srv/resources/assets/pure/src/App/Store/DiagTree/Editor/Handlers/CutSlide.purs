module App.Store.DiagTree.Editor.Handlers.CutSlide
     ( cutSlide
     ) where

import Prelude

import Data.Maybe (maybe, fromMaybe)

import Control.Monad.Error.Class (catchError, throwError)

import Effect.Aff (Aff)
import Effect.Exception (error, message, stack)

import App.Store (Store)
import App.Store.Actions (AppAction)
import App.Store.DiagTree.Editor.Handlers.Helpers (errLog, sendAction)
import App.Store.DiagTree.Editor.Reducers (DiagTreeEditorState)

import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction ( CutSlideSuccess
                            , CutSlideFailure
                            )
     )


cutSlide
  :: forall state
   . Store state AppAction
  -> DiagTreeEditorState
  -> Aff Unit

cutSlide store state = catchError go handleError where
  go =
    case slidePath of
      [ ] -> throwError $ error "Slide path is empty"
      _   -> act $ CutSlideSuccess slidePath

  slidePath = fromMaybe [] state.copyPasteBuffer.branch
  act = sendAction store

  reportErr err = errLog $
    "Cutting slide " <> show slidePath <> " failed: " <> message err
    # \x -> maybe x (\y -> x <> "\nStacktrace:\n" <> y) (stack err)

  handleError err = do
    reportErr err
    act $ CutSlideFailure slidePath
