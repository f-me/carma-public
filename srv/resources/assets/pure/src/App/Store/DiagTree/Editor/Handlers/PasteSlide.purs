module App.Store.DiagTree.Editor.Handlers.PasteSlide
     ( pasteSlide
     ) where

import Prelude
import Data.Array (null)

import App.Store (AppContext)
import App.Store.DiagTree.Editor.Actions (DiagTreeEditorAction(LoadSlidesRequest, PasteSlideSuccess, PasteSlideFailure))
import App.Store.DiagTree.Editor.Handlers.Helpers (errLog, sendAction)
import App.Store.DiagTree.Editor.Reducers (DiagTreeEditorState)
import App.Store.DiagTree.Editor.Types (DiagTreeSlideId, DiagTreeSlides, DiagTreeSlide(DiagTreeSlide))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (error, message, stack)
import Control.Monad.Error.Class (catchError, throwError)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Network.HTTP.Affjax (AJAX, AffjaxResponse, affjax)


pasteSlide
  :: forall eff
   . AppContext
  -> DiagTreeEditorState
  -> Array DiagTreeSlideId
  -> Aff (avar :: AVAR, console :: CONSOLE, ajax :: AJAX | eff) Unit

pasteSlide appCtx state destinationSlidePath = flip catchError handleError $ do
  if null sourceSlidePath
    then do logShow "pasteSlide: source slide path is empty"
            act $ PasteSlideFailure sourceSlidePath
    else if null destinationSlidePath
         then do logShow "pasteSlide: destination slide path is empty"
                 act $ PasteSlideFailure destinationSlidePath
         else do logShow $ "pasteSlide: paste " <> show sourceSlidePath <> " into " <> show destinationSlidePath
                 act $ PasteSlideSuccess []

  where
    sourceSlidePath = fromMaybe [] state.copyPasteBuffer.branch

    act = sendAction appCtx

    reportErr err = errLog $
      "Paste slide (" <> show sourceSlidePath <> ") failed: " <> message err
      # \x -> maybe x (\y -> x <> "\nStacktrace:\n" <> y) (stack err)

    handleError err = do
      reportErr err
      act $ PasteSlideFailure sourceSlidePath
