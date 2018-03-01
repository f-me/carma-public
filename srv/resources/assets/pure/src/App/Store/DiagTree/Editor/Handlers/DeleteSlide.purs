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

import Data.Maybe (Maybe (..), maybe)
import Data.Either (Either (..), either)
import Data.Foreign (Foreign)
import Data.Array (snoc)
import Data.Foldable (foldl, foldM)
import Data.StrMap as StrMap
import Data.Argonaut.Core as A

import Network.HTTP.Affjax (AJAX, AffjaxResponse, affjax)

import Utils.Affjax (putRequest)
import Utils.DiagTree.Editor (getSlideByBranch)
import App.Store (AppContext)
import App.Store.DiagTree.Editor.Handlers.Helpers (errLog, sendAction)

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

       [x] -> do
         let isSlideRoot (DiagTreeSlide { isRoot }) = isRoot
         case getSlideByBranch slides slidePath <#> isSlideRoot of
              Nothing -> throwError $ error $
                "Slide #" <> show x <> " not found"
              Just false -> throwError $ error $
                "Slide #" <> show x <> " is not root slide"
              Just true -> scenarioOne x

       _ -> scenarioTwo

  act $ DeleteSlideSuccess slidePath
  act $ LoadSlidesRequest -- Reloading slides again

  where
    act = sendAction appCtx

    reportErr err = errLog $
      "Deleting slide (" <> show slidePath <> ") failed: " <> message err
      # \x -> maybe x (\y -> x <> "\nStack trace:\n" <> y) (stack err)

    handleError err = do
      reportErr err
      act $ DeleteSlideFailure slidePath

    -- When we have just root slide selected.
    -- In this case we're just deactivating this root slide.
    scenarioOne slideId = do
      (_ :: AffjaxResponse Foreign) <- affjax $
        putRequest ("/_/DiagSlide/" <> show slideId) # _
          { content = Just $ A.fromObject $
              StrMap.singleton "isActive" $ A.fromBoolean false
          }

      pure unit

    -- When we have selected nested branch.
    -- We're about to remove an answer from parent slide
    -- and to deactivate slide we're deleting.
    scenarioTwo = do
      (_ :: Array (AffjaxResponse Foreign)) <-
        extractParResult $
          sequential $ foldl (\acc x -> snoc <$> acc <*> x) (pure [])
            [ parallel $ catchPar $ affjax $ putRequest "/_/DiagSlide/TODO_ID"
            , parallel $ catchPar $ affjax $ putRequest "/_/DiagSlide/TODO_ID"
            ]

      pure unit

      where
        catchPar m = catchError (Right <$> m) (Left >>> pure)

        extractParResult =
          (_ >>= foldM (\acc x -> x # either throwError pure <#> snoc acc) [])
