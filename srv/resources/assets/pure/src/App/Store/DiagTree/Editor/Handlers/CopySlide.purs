module App.Store.DiagTree.Editor.Handlers.CopySlide
     ( copySlide
     ) where

import Prelude

import Data.Maybe (maybe, fromMaybe)

import Control.Monad.Error.Class (catchError, throwError)

import Effect.Aff (Aff)
import Effect.Exception (error, message, stack)

import App.Store (AppContext)
import App.Store.DiagTree.Editor.Handlers.Helpers (errLog, sendAction)
import App.Store.DiagTree.Editor.Reducers (DiagTreeEditorState)

import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction ( CopySlideSuccess
                            , CopySlideFailure
                            )
     )


copySlide :: AppContext -> DiagTreeEditorState -> Aff Unit
copySlide appCtx state = catchError go handleError where
  go =
    case slidePath of
      [ ] -> throwError $ error "Slide path is empty"
      _   -> act $ CopySlideSuccess slidePath

  slidePath = fromMaybe [] state.copyPasteBuffer.branch
  act = sendAction appCtx

  reportErr err = errLog $
    "Copyng slide " <> show slidePath <> " failed: " <> message err
    # \x -> maybe x (\y -> x <> "\nStacktrace:\n" <> y) (stack err)

  handleError err = do
    reportErr err
    act $ CopySlideFailure slidePath
