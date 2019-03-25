module App.Store.DiagTree.Editor.Handlers.NewSlide
     ( newSlide
     ) where

import Prelude

import Data.Either (Either (..))
import Data.Maybe (Maybe (..), maybe)
import Data.Argonaut.Core as A
import Data.Map as Map

import Control.Monad.Trans.Class (lift)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Maybe.Trans (runMaybeT)

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error, message, stack)

import Affjax (request)
import Affjax.RequestBody as RequestBody

import Affjax.ResponseFormat
     ( ResponseFormatError, printResponseFormatError, json
     )

import Utils (toMaybeT)
import Utils.Affjax (getRequest, postRequest)
import App.Store (AppContext)
import App.Store.DiagTree.Editor.Handlers.Helpers (errLog, sendAction)
import App.Store.DiagTree.Editor.Types (DiagTreeSlide (DiagTreeSlide))

import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction (NewSlideSuccess, NewSlideFailure, SelectSlide)
     )

import App.Store.DiagTree.Editor.Handlers.SharedUtils
     ( defaultPartialBackendSlide
     , toBackendSlideFromPartial
     , fromBackendSlide
     , getSlide
     , getBackendSlideId
     )


newSlide :: AppContext -> Aff Unit
newSlide appCtx = catchError go handleError where
  go = do
    (newSlideResponse :: Either ResponseFormatError A.Json) <-
      map _.body $ request $ postRequest newSlideUrl jsonRequest json

    newSlideCreateResult <-
      case newSlideResponse of
           Right x -> pure x
           Left  e -> reportParseError e

    slide <- runMaybeT $ do
      newSlideId <- pure $ getBackendSlideId newSlideCreateResult

      (getSlideResponse :: Either ResponseFormatError A.Json) <- lift $
        map _.body $ request $ getRequest (getSlideUrl newSlideId) json

      getSlideJson <-
        case getSlideResponse of
             Right x -> lift $ pure x
             Left  e -> lift $ reportParseError e

      x <- toMaybeT $ fromBackendSlide getSlideJson
      y <- liftEffect $ runMaybeT $ getSlide (Map.singleton x.id x) x.id
      toMaybeT y

    case slide of
         Nothing -> throwError $ error "Parsing new created slide failed"
         Just x@(DiagTreeSlide { id: newSlideId }) -> do
           act $ NewSlideSuccess x
           act $ SelectSlide [newSlideId]

  act = sendAction appCtx
  newSlideUrl = "/_/DiagSlide"
  getSlideUrl slideId = "/_/DiagSlide/" <> show slideId

  jsonRequest
    = RequestBody.Json
    $ toBackendSlideFromPartial
    $ defaultPartialBackendSlide
        { isRoot    = Just true
        , header    = Just "Новый вопрос"
        , body      = Just "?"
        , resources = Just []
        , actions   = Just []
        , answers   = Just []
        }

  handleError err = do
    reportErr err
    act NewSlideFailure

  reportErr err = errLog $
    "Creating new slide failed: " <> message err
    # \x -> maybe x (\y -> x <> "\nStack trace:\n" <> y) (stack err)

  reportParseError formatErr = liftEffect $ throwError $ error $
    "Parsing response of creating new slide is failed " <>
    "(error: " <> printResponseFormatError formatErr <> ")!"
