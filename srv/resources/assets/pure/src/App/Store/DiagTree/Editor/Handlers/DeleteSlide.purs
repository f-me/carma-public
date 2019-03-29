module App.Store.DiagTree.Editor.Handlers.DeleteSlide
     ( deleteSlide
     ) where

import Prelude

import Data.Tuple (Tuple (Tuple), uncurry)
import Data.Maybe (Maybe (..), maybe)
import Data.Either (Either (..), either)
import Data.Array (snoc, unsnoc, filter)
import Data.Foldable (foldl, foldM)
import Data.Argonaut.Core as A

import Control.Parallel (sequential, parallel)
import Control.Monad.Error.Class (catchError, throwError)

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error, message, stack)

import Affjax (request)
import Affjax.RequestBody as RequestBody

import Affjax.ResponseFormat
     ( ResponseFormatError, printResponseFormatError, json
     )

import Utils.Affjax (getRequest, putRequest)
import Utils.DiagTree.Editor (getSlideByBranch)
import App.Store (AppContext)
import App.Store.DiagTree.Editor.Handlers.Helpers (errLog, sendAction)

import App.Store.DiagTree.Editor.Handlers.SharedUtils
     ( fromBackendSlide
     , toBackendSlideFromPartial
     , defaultPartialBackendSlide
     )

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlideId
     , DiagTreeSlides
     , DiagTreeSlide (DiagTreeSlide)
     )

import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction ( LoadSlidesRequest
                            , DeleteSlideSuccess
                            , DeleteSlideFailure
                            )
     )


deleteSlide :: AppContext -> DiagTreeSlides -> Array DiagTreeSlideId -> Aff Unit
deleteSlide appCtx slides slidePath = catchError go handleError where
  go = do
    case slidePath of

         [ ] -> throwError $ error "Slide path is empty"

         [x] ->
           let isSlideRoot (DiagTreeSlide { isRoot }) = isRoot
            in case getSlideByBranch slides slidePath <#> isSlideRoot of
                    Just true -> scenarioOne x
                    Just false -> throwError $ error $
                      "Slide #" <> show x <> " is not a root slide"
                    Nothing -> throwError $ error $
                      "Slide #" <> show x <> " not found"

         x ->
           let result =
                 unsnoc x >>= \ { init, last } ->
                   Tuple <$> (unsnoc init <#> _.last) <*> pure last

            in case result of
                    Just y  -> uncurry scenarioTwo y
                    Nothing -> throwError $ error $
                      "Cannot get parent for slide #" <> show x

    act $ DeleteSlideSuccess slidePath
    act LoadSlidesRequest -- Reloading slides again

  act = sendAction appCtx
  slideUrl slideId = "/_/DiagSlide/" <> show slideId

  reportErr err = errLog $
    "Deleting slide (" <> show slidePath <> ") failed: " <> message err
    # \x -> maybe x (\y -> x <> "\nStack trace:\n" <> y) (stack err)

  handleError err = do
    reportErr err
    act $ DeleteSlideFailure slidePath

  deactivateSlide slideId = go' where
    go' = do
      (res :: Either ResponseFormatError A.Json) <-
        map _.body $ request $ putRequest (slideUrl slideId) jsonRequest json

      case res of
           Right x -> pure x
           Left  e -> reportParseError e

    jsonRequest
      = RequestBody.Json
      $ toBackendSlideFromPartial
      $ defaultPartialBackendSlide { isActive = Just false }

    reportParseError formatErr = liftEffect $ throwError $ error $
      "Parsing response of deactivating slide (" <> show slideId <> ") " <>
      " is failed (error: " <> printResponseFormatError formatErr <> ")!"

  -- When we have just root slide selected.
  -- In this case we're just deactivating this root slide.
  scenarioOne = void <<< deactivateSlide

  -- When we have selected nested branch.
  -- We're about to remove an answer from parent slide
  -- and to deactivate slide we're deleting.
  scenarioTwo parentSlideId slideId = go' where
    go' = do
      (res :: Either ResponseFormatError A.Json) <-
        map _.body $ request $ getRequest (slideUrl parentSlideId) json

      -- Getting new answers for parent slide with removed answer with slide
      -- we're "deleting" (actually just deactivating).
      newAnswersArray <-
        case res of
             Left  e -> reportParseGetError e
             Right x ->
               case fromBackendSlide x of
                    Just y -> pure $
                      filter (\z -> z.nextSlide /= slideId) y.answers

                    Nothing -> throwError $ error $
                      "Parsing 'answers' of parent slide #"
                        <> show parentSlideId
                        <> " of slide #"
                        <> show slideId
                        <> " error"

      let newParentAnswersJsonRequest
            = RequestBody.Json
            $ toBackendSlideFromPartial
            $ defaultPartialBackendSlide { answers = Just newAnswersArray }

      let updateParentReq = updateParentSlideAnswers newParentAnswersJsonRequest

      (_ :: Array A.Json) <-
        extractParResult $
          -- Deactivating a slide and removing that slide from answers list
          -- of its parent slide in parallel.
          sequential $ foldl (\acc x -> snoc <$> acc <*> x) (pure [])
            [ parallel $ catchPar $ updateParentReq
            , parallel $ catchPar $ deactivateSlide slideId
            ]

      pure unit

    updateParentSlideAnswers reqBody = do
      (res :: Either ResponseFormatError A.Json) <-
        map _.body $ request $ putRequest (slideUrl parentSlideId) reqBody json

      case res of
           Right x -> pure x
           Left  e -> reportParseUpdateError e

    catchPar m = catchError (Right <$> m) (Left >>> pure)

    extractParResult =
      (_ >>= foldM (\acc x -> x # either throwError pure <#> snoc acc) [])

    reportParseGetError formatErr = liftEffect $ throwError $ error $
      "Parsing response of getting slide #" <> show parentSlideId <> " " <>
      "(which is parent of #" <> show slideId <> " slide) is failed " <>
      "(error: " <> printResponseFormatError formatErr <> ")!"

    reportParseUpdateError formatErr = liftEffect $ throwError $ error $
      "Parsing response of updating parent slide #" <> show parentSlideId <>
      " (of #" <> show slideId <> " slide) answers is failed" <>
      " (error: " <> printResponseFormatError formatErr <> ")!"
