module App.Store.DiagTree.Editor.Handlers.SaveSlide
     ( saveSlide
     ) where

import Prelude

import Data.Array (snoc, zipWith, concat)
import Data.Maybe (Maybe (..), maybe)
import Data.Either (Either (..), either)
import Data.Foldable (foldl, foldM)
import Data.Argonaut.Core as A

import Control.Monad.Error.Class (catchError, throwError)
import Control.Parallel.Class (sequential, parallel)

import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error, message, stack)

import Affjax (request)
import Affjax.RequestBody as RequestBody

import Affjax.ResponseFormat
     ( ResponseFormatError, printResponseFormatError, json, ignore
     )

import Utils.Affjax (postRequest, putRequest)
import App.Store (Store)
import App.Store.Actions (AppAction)
import App.Store.DiagTree.Editor.Handlers.Helpers (errLog, sendAction)

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlideId
     , DiagTreeSlide (DiagTreeSlide)
     , DiagTreeSlideAttachment (..)
     )

import App.Store.DiagTree.Editor.Handlers.SharedUtils
     ( BackendAnswer
     , defaultPartialBackendSlide
     , extractPartialBackendSlideFromSlide
     , toBackendSlideFromPartial
     , getBackendSlideId
     )

import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction ( LoadSlidesRequest
                            , SaveSlideSuccess
                            , SaveSlideFailure
                            )
     )


saveSlide
  :: forall state
   . Store state AppAction
  -> Array DiagTreeSlideId
  -> DiagTreeSlide
  -> Array { header     :: String
           , text       :: String
           , attachment :: Maybe DiagTreeSlideAttachment
           }
  -> Aff Unit

saveSlide store slidePath slide@(DiagTreeSlide s) newAnswers = go where
  go = catchError process handleError

  process = do
    (completeNewAnswers :: Array BackendAnswer) <-
      newSlides <#> flip zipWith newAnswers \a nextSlide ->
        { nextSlide
        , header: a.header
        , text: a.text

        , attachment:
            case a.attachment of
                 Just (Modern x) -> Just x
                 _ -> Nothing

        , file:
            case a.attachment of
                 Just (Legacy x) -> Just x
                 _ -> Nothing
        }

    let slideForBackend =
          let x = extractPartialBackendSlideFromSlide slide
           in x { answers = x.answers <#> \y -> concat [y, completeNewAnswers] }

    _ <-
      let reqBody = RequestBody.Json $ toBackendSlideFromPartial slideForBackend
       in request $ putRequest ("/_/DiagSlide/" <> show s.id) reqBody ignore

    act $ SaveSlideSuccess slidePath
    act LoadSlidesRequest -- Reloading updated slides

  act = sendAction store
  handleError err = reportErr err *> act (SaveSlideFailure slidePath)

  reportErr err = errLog $
    "Saving slide (" <> show slidePath <> ") failed: " <> message err
    # \x -> maybe x (\y -> x <> "\nStack trace:\n" <> y) (stack err)

  newSlide = do
    (newSlideResponse :: Either ResponseFormatError A.Json) <-
      let
        reqBody = RequestBody.Json $
          toBackendSlideFromPartial $ defaultPartialBackendSlide
            { isRoot    = Just false
            , header    = Just "…"
            , body      = Just "…"
            , resources = Just []
            , actions   = Just []
            , answers   = Just []
            }
      in
        liftAff $ map _.body $ request $
          postRequest "/_/DiagSlide" reqBody json

    case newSlideResponse of
         Right newSlideJson ->
           case getBackendSlideId newSlideJson of
                Just  x -> pure x
                Nothing ->
                  throwError $ error $
                    "Extracting slide id from server response is failed\
                    \ (response body: " <> A.stringify newSlideJson <> ")"

         Left formatErr ->
           throwError $ error $
             "Parsing new created slide failed (error: " <>
             printResponseFormatError formatErr <> ")"

  newSlides = go' where
    go' =
      extractParResult $
        sequential $ foldl (\acc x -> snoc <$> acc <*> x) (pure []) $
          (const $ parallel $ catchPar newSlide) <$> newAnswers

    catchPar m = catchError (Right <$> m) (Left >>> pure)

    extractParResult =
      (_ >>= foldM (\acc x -> x # either throwError pure <#> snoc acc) [])
