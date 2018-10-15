module App.Store.DiagTree.Editor.Handlers.PasteSlide
     ( pasteSlide
     ) where

import Prelude
import Data.Array (null)

import App.Store (AppContext)
import App.Store.DiagTree.Editor.Actions (DiagTreeEditorAction(LoadSlidesRequest, PasteSlideSuccess, PasteSlideFailure))
import App.Store.DiagTree.Editor.Handlers.Helpers (errLog, sendAction)
import App.Store.DiagTree.Editor.Reducers (DiagTreeEditorState)
import App.Store.DiagTree.Editor.Types (DiagTreeSlideId)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (message, stack)
import Control.Monad.Error.Class (catchError)
import Data.Argonaut.Core as A
import Data.Foreign (Foreign)
import Data.Int (toNumber)
import Data.Maybe (maybe, fromMaybe)
import Data.Tuple (Tuple (Tuple))
import Data.StrMap as StrMap
import Network.HTTP.Affjax (AJAX, AffjaxResponse, affjax)
import Utils.Affjax (postRequest)


pasteSlide
  :: forall eff
   . AppContext
  -> DiagTreeEditorState
  -> Array DiagTreeSlideId
  -> Aff (avar :: AVAR, console :: CONSOLE, ajax :: AJAX | eff) Unit

pasteSlide appCtx state destinationSlidePath = flip catchError handleError $ do
  if null sourceSlidePath
    then act $ PasteSlideFailure sourceSlidePath
    else if null destinationSlidePath
         then act $ PasteSlideFailure destinationSlidePath
         else do (res :: AffjaxResponse Foreign) <- affjax $ postRequest url jsonRequest
                 act $ PasteSlideSuccess destinationSlidePath
                 act LoadSlidesRequest -- Reload slides again

  where
    url = if state.copyPasteBuffer.cutting
          then "/diag/slide/move"
          else "/diag/slide/copy"

    jsonRequest = A.fromObject $
                  StrMap.fromFoldable [ Tuple "source" $ A.fromArray $ map A.fromNumber $ map toNumber sourceSlidePath
                                      , Tuple "destination" $ A.fromArray $ map A.fromNumber $ map toNumber destinationSlidePath
                                      ]

    sourceSlidePath = fromMaybe [] state.copyPasteBuffer.branch

    act = sendAction appCtx

    reportErr err = errLog $
      "Paste slide (" <> show sourceSlidePath <> ") failed: " <> message err
      # \x -> maybe x (\y -> x <> "\nStacktrace:\n" <> y) (stack err)

    handleError err = do
      reportErr err
      act $ PasteSlideFailure sourceSlidePath
