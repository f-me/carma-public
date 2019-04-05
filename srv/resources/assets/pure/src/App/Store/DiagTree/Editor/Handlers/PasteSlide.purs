module App.Store.DiagTree.Editor.Handlers.PasteSlide
     ( pasteSlide
     ) where

import Prelude

import Data.Array (null)
import Data.Argonaut.Core as A
import Data.Int (toNumber, fromNumber)
import Data.Maybe (Maybe (..), maybe, fromMaybe)
import Data.Either (Either (..))
import Data.Tuple (Tuple (Tuple))
import Data.Traversable (sequence)
import Foreign.Object as FObj

import Control.Monad.Error.Class (throwError, catchError)

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error, message, stack)

import Affjax (request)
import Affjax.RequestBody as RequestBody

import Affjax.ResponseFormat
     ( ResponseFormatError, printResponseFormatError, json
     )

import Utils.Affjax (postRequest)
import App.Store (Store)
import App.Store.Actions (AppAction)
import App.Store.DiagTree.Editor.Handlers.Helpers (errLog, sendAction)
import App.Store.DiagTree.Editor.Reducers (DiagTreeEditorState)
import App.Store.DiagTree.Editor.Types (DiagTreeSlideId)

import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction ( LoadSlidesRequest
                            , PasteSlideSuccess
                            , PasteSlideFailure
                            , SelectSlide
                            )
     )


pasteSlide
  :: forall state
   . Store state AppAction
  -> DiagTreeEditorState
  -> Array DiagTreeSlideId
  -> Aff Unit

pasteSlide store state destinationSlidePath = catchError go handleError where
  go = do
    if null sourceSlidePath
       then act $ PasteSlideFailure sourceSlidePath
       else do (newPathResponse :: Either ResponseFormatError A.Json) <-
                 map _.body $ request $ postRequest url jsonRequest json

               newPathJSON <-
                 case newPathResponse of
                      Right x -> pure x
                      Left  e -> reportParseError e

               selectedSlidePath <-
                 case A.toArray newPathJSON of
                      Nothing -> pure $ destinationSlidePath
                      Just x  ->
                        case sequence $ map decodeInt x of
                             Just y  -> pure y
                             Nothing -> liftEffect $ throwError $ error $
                               "New slide path got from server is incorrect " <>
                               "(must be an array of numbers)!"

               act $ PasteSlideSuccess destinationSlidePath
               act LoadSlidesRequest -- Reload slides again
               act $ SelectSlide selectedSlidePath

  url =
    if state.copyPasteBuffer.cutting
       then "/diag/slide/move"
       else "/diag/slide/copy"

  jsonRequest
    = RequestBody.Json
    $ A.fromObject
    $ FObj.fromFoldable
    [ Tuple "source"      $ pathToJSON sourceSlidePath
    , Tuple "destination" $ pathToJSON destinationSlidePath
    ]

  sourceSlidePath = fromMaybe [] state.copyPasteBuffer.branch
  pathToJSON = A.fromArray <<< map A.fromNumber <<< map toNumber
  decodeInt = A.toNumber >=> fromNumber
  act = sendAction store

  handleError err = do
    reportErr err
    act $ PasteSlideFailure sourceSlidePath

  reportErr err = errLog $
    "Paste slide (" <> show sourceSlidePath <> ") is failed: " <> message err
    # \x -> maybe x (\y -> x <> "\nStack trace:\n" <> y) (stack err)

  reportParseError formatErr = liftEffect $ throwError $ error $
    "Parsing response of pasting slide (" <> show sourceSlidePath <> ") to " <>
    show destinationSlidePath <> " destination slide path is failed " <>
    "(error: " <> printResponseFormatError formatErr <> ")!"
