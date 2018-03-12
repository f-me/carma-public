module App.Store.DiagTree.Editor.Handlers.SaveSlide
     ( saveSlide
     ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (error, message, stack)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Parallel (sequential, parallel)

import Data.Array (snoc, zipWith, concat)
import Data.Maybe (Maybe (..), maybe)
import Data.Either (Either (..), either)
import Data.Foldable (foldl, foldM)
import Data.Foreign (Foreign, unsafeFromForeign)
import Data.Argonaut.Core as A

import Network.HTTP.Affjax (AJAX, AffjaxResponse, affjax)

import Utils.Affjax (postRequest, putRequest)
import App.Store (AppContext)
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
  :: forall eff
   . AppContext
  -> Array DiagTreeSlideId
  -> DiagTreeSlide
  -> Array { header     :: String
           , text       :: String
           , attachment :: Maybe DiagTreeSlideAttachment
           }
  -> Aff (avar :: AVAR, console :: CONSOLE, ajax :: AJAX | eff) Unit

saveSlide appCtx slidePath slide@(DiagTreeSlide s) newAnswers =
  flip catchError handleError $ do

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

    (_ :: AffjaxResponse Foreign) <-
      affjax $ putRequest ("/_/DiagSlide/" <> show s.id) $
        toBackendSlideFromPartial slideForBackend

    act $ SaveSlideSuccess slidePath
    act LoadSlidesRequest -- Reloading updated slides

  where
    act = sendAction appCtx
    handleError err = reportErr err *> act (SaveSlideFailure slidePath)

    reportErr err = errLog $
      "Saving slide (" <> show slidePath <> ") failed: " <> message err
      # \x -> maybe x (\y -> x <> "\nStack trace:\n" <> y) (stack err)

    newSlide = do
      (newSlideRes :: AffjaxResponse Foreign) <-
        liftAff $ affjax $ postRequest "/_/DiagSlide" $
          toBackendSlideFromPartial $ defaultPartialBackendSlide
            { isRoot    = Just false
            , header    = Just "…"
            , body      = Just "…"
            , resources = Just []
            , actions   = Just []
            , answers   = Just []
            }

      let newSlideJson = unsafeFromForeign newSlideRes.response :: A.Json

      case getBackendSlideId newSlideJson of
           Nothing -> throwError $ error "Parsing new created slide failed"
           Just x  -> pure x

    newSlides =
      extractParResult $
        sequential $ foldl (\acc x -> snoc <$> acc <*> x) (pure []) $
          (const $ parallel $ catchPar newSlide) <$> newAnswers

      where
        catchPar m = catchError (Right <$> m) (Left >>> pure)

        extractParResult =
          (_ >>= foldM (\acc x -> x # either throwError pure <#> snoc acc) [])
