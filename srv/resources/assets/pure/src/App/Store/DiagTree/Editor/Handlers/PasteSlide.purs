module App.Store.DiagTree.Editor.Handlers.PasteSlide
     ( pasteSlide
     ) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (error, message, stack)
import Control.Monad.Error.Class (catchError, throwError)
import Network.HTTP.Affjax (AJAX, AffjaxResponse, affjax)
import Data.Maybe (Maybe (..), maybe)

import App.Store (AppContext)
import App.Store.DiagTree.Editor.Handlers.Helpers (errLog, sendAction)
import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlideId
     , DiagTreeSlides
     , DiagTreeSlide (DiagTreeSlide)
     )
import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction ( LoadSlidesRequest
                            , PasteSlideSuccess
                            , PasteSlideFailure
                            )
     )


pasteSlide
  :: forall eff
   . AppContext
  -> DiagTreeSlides
  -> Array DiagTreeSlideId
  -> Aff (avar :: AVAR, console :: CONSOLE, ajax :: AJAX | eff) Unit

pasteSlide appCtx slides slidePath = flip catchError handleError $ do
  logShow $ "pasteSlide: slidePath=" <> show slidePath

  where
    act = sendAction appCtx

    reportErr err = errLog $
      "Paste slide (" <> show slidePath <> ") failed: " <> message err
      # \x -> maybe x (\y -> x <> "\nStacktrace:\n" <> y) (stack err)

    handleError err = do
      reportErr err
      act $ PasteSlideFailure slidePath
