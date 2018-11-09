module App.Store.DiagTree.Editor.Handlers.PasteSlide
     ( pasteSlide
     ) where

import Prelude
import Data.Array (null)

import App.Store (AppContext)
import App.Store.DiagTree.Editor.Actions
       ( DiagTreeEditorAction ( LoadSlidesRequest
                              , PasteSlideSuccess
                              , PasteSlideFailure
                              , SelectSlide
                              )
       )
import App.Store.DiagTree.Editor.Handlers.Helpers (errLog, sendAction)
import App.Store.DiagTree.Editor.Reducers (DiagTreeEditorState)
import App.Store.DiagTree.Editor.Types (DiagTreeSlideId)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (message, stack)
import Control.Monad.Error.Class (catchError)
import Data.Argonaut.Core as A
import Data.Foreign (Foreign, unsafeFromForeign)
import Data.Int (toNumber, fromNumber)
import Data.Maybe (Maybe(..), maybe, fromMaybe, fromJust)
import Data.Tuple (Tuple (Tuple))
import Data.StrMap as StrMap
import Network.HTTP.Affjax (AJAX, AffjaxResponse, affjax)
import Utils.Affjax (postRequest)
import Partial.Unsafe (unsafePartial)


pasteSlide
  :: forall eff
   . AppContext
  -> DiagTreeEditorState
  -> Array DiagTreeSlideId
  -> Aff (avar :: AVAR, console :: CONSOLE, ajax :: AJAX | eff) Unit

pasteSlide appCtx state destinationSlidePath = flip catchError handleError $ do
  if null sourceSlidePath
    then act $ PasteSlideFailure sourceSlidePath
    else do (newPathResponse :: AffjaxResponse Foreign) <-
              affjax $ postRequest url jsonRequest
            let newPathJSON = unsafeFromForeign newPathResponse.response
            let selectedSlidePath =
                  case A.toArray newPathJSON of
                    Just a -> map decodeInt a
                    Nothing -> destinationSlidePath
            act $ PasteSlideSuccess destinationSlidePath
            act LoadSlidesRequest -- Reload slides again
            act $ SelectSlide selectedSlidePath

  where
    url = if state.copyPasteBuffer.cutting
          then "/diag/slide/move"
          else "/diag/slide/copy"

    jsonRequest =
      A.fromObject $
      StrMap.fromFoldable [ Tuple "source" $ pathToJSON sourceSlidePath
                          , Tuple "destination" $ pathToJSON destinationSlidePath
                          ]

    sourceSlidePath = fromMaybe [] state.copyPasteBuffer.branch

    pathToJSON = A.fromArray <<< map A.fromNumber <<< map toNumber

    decodeInt x = unsafePartial $ fromJust $ fromNumber $
                  unsafePartial $ fromJust $ A.toNumber x

    act = sendAction appCtx

    reportErr err = errLog $
      "Paste slide (" <> show sourceSlidePath <> ") failed: " <> message err
      # \x -> maybe x (\y -> x <> "\nStacktrace:\n" <> y) (stack err)

    handleError err = do
      reportErr err
      act $ PasteSlideFailure sourceSlidePath
