module App.Store.DiagTree.Editor.Handlers.DeleteSlide
     ( deleteSlide
     ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (error, message, stack)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Parallel (sequential, parallel)

import Data.Tuple (Tuple (Tuple), uncurry)
import Data.Maybe (Maybe (..), maybe)
import Data.Either (Either (..), either)
import Data.Foreign (Foreign, unsafeFromForeign)
import Data.Array (snoc, unsnoc, filter)
import Data.Foldable (foldl, foldM)
import Data.Argonaut.Core as A

import Network.HTTP.Affjax (AJAX, AffjaxResponse, affjax)

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


deleteSlide
  :: forall eff
   . AppContext
  -> DiagTreeSlides
  -> Array DiagTreeSlideId
  -> Aff (avar :: AVAR, console :: CONSOLE, ajax :: AJAX | eff) Unit

deleteSlide appCtx slides slidePath = flip catchError handleError $ do

  case slidePath of

       [ ] -> throwError $ error "Slide path is empty"

       [x] ->
         let isSlideRoot (DiagTreeSlide { isRoot }) = isRoot
          in case getSlideByBranch slides slidePath <#> isSlideRoot of
                  Just true -> scenarioOne x
                  Just false -> throwError $ error $
                    "Slide #" <> show x <> " is not root slide"
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

  where
    act = sendAction appCtx

    reportErr err = errLog $
      "Deleting slide (" <> show slidePath <> ") failed: " <> message err
      # \x -> maybe x (\y -> x <> "\nStack trace:\n" <> y) (stack err)

    handleError err = do
      reportErr err
      act $ DeleteSlideFailure slidePath

    deactivateSlide slideId = do
      (res :: AffjaxResponse Foreign) <- affjax $
        putRequest ("/_/DiagSlide/" <> show slideId) $
          toBackendSlideFromPartial $ defaultPartialBackendSlide
            { isActive = Just false }

      pure res

    -- When we have just root slide selected.
    -- In this case we're just deactivating this root slide.
    scenarioOne slideId = void $ deactivateSlide slideId

    -- When we have selected nested branch.
    -- We're about to remove an answer from parent slide
    -- and to deactivate slide we're deleting.
    scenarioTwo parentSlideId slideId = do
      (res :: AffjaxResponse Foreign) <- affjax $
        getRequest ("/_/DiagSlide/" <> show parentSlideId)

      let json = unsafeFromForeign res.response :: A.Json

          newAnswers =
            fromBackendSlide json
            <#> _.answers
            <#> filter (\x -> x.nextSlide /= slideId)

      newAnswersArray <-
        case newAnswers of
             Just x  -> pure x
             Nothing -> throwError $ error $
               "Parsing 'answers' of parent slide #"
                 <> show parentSlideId
                 <> " of slide #"
                 <> show slideId
                 <> " error"

      (_ :: Array (AffjaxResponse Foreign)) <-
        extractParResult $
          sequential $ foldl (\acc x -> snoc <$> acc <*> x) (pure [])
            [ parallel $ catchPar $ affjax $
                putRequest ("/_/DiagSlide/" <> show parentSlideId) $
                  toBackendSlideFromPartial $ defaultPartialBackendSlide
                    { answers = Just newAnswersArray }

            , parallel $ catchPar $ deactivateSlide slideId
            ]

      pure unit

      where
        catchPar m = catchError (Right <$> m) (Left >>> pure)

        extractParResult =
          (_ >>= foldM (\acc x -> x # either throwError pure <#> snoc acc) [])
