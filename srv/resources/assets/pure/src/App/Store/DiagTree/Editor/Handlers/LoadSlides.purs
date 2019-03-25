module App.Store.DiagTree.Editor.Handlers.LoadSlides
     ( loadSlides
     ) where

import Prelude

import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (Tuple), fst, snd)
import Data.Array (snoc)
import Data.Foldable (foldM, foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Argonaut.Core as A
import Foreign.Object as FObj

import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)

import Effect.Aff (Aff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (error, message)

import Affjax (request)

import Affjax.ResponseFormat
     ( ResponseFormatError, printResponseFormatError, json
     )

import Utils (toMaybeT)
import Utils.Affjax (getRequest)
import App.Store (AppContext)
import App.Store.DiagTree.Editor.Handlers.Helpers (errLog, sendAction)
import App.Store.DiagTree.Editor.Types (DiagTreeSlides, DiagTreeSlideId)
import App.Store.DiagTree.Editor.Handlers.SharedUtils.Slide (getSlide)

import App.Store.DiagTree.Editor.Handlers.SharedUtils
     ( BackendSlide, fromBackendSlideObj
     )

import App.Store.DiagTree.Editor.Actions
     ( LoadSlidesFailureReason (..)
     , DiagTreeEditorAction (LoadSlidesSuccess, LoadSlidesFailure)
     )


loadSlides :: AppContext -> Aff Unit
loadSlides appCtx = catchError go handleError where
  go = do
    (res :: Either ResponseFormatError A.Json) <-
      map _.body $ request $ getRequest "/_/DiagSlide" json

    json' <-
      case res of
           Right x -> pure x
           Left  e -> reportParseError e

    parsed <- flip catchError handleParseError $ do
      let -- Parsing single element (called "flat slide" or "backend slide").
          -- Not `DiagTreeSlide` yet, just a record of a slide for flat map.
          parseFlatElem acc x = do
            flatSlide@{ id, isRoot } <- fromBackendSlideObj x

            let newAcc = if isRoot && fst acc == Nothing
                            then Tuple (Just id) $ snd acc
                            else acc

            pure $ flip map newAcc $ Map.insert id flatSlide

          -- Building flat slides map
          reduceFlat acc jsonItem = do
            x        <- A.toObject jsonItem
            isActive <- FObj.lookup "isActive" x >>= A.toBoolean

            if not isActive
               then pure acc -- Ignore inactive slides
               else parseFlatElem acc x

          parsedFlat =
            A.toArray json'
            >>= foldM reduceFlat (Tuple Nothing Map.empty)
            >>= \(Tuple rootSlide flatSlides) ->
                  case rootSlide of
                       Just x  -> Just $ Tuple x flatSlides
                       Nothing -> Nothing


          reduceTree
            :: Map DiagTreeSlideId BackendSlide
            -> DiagTreeSlides
            -> DiagTreeSlideId
            -> MaybeT Effect DiagTreeSlides

          reduceTree flatSlides acc slideId =
            getSlide flatSlides slideId <#> flip (Map.insert slideId) acc


          -- `Effect` is required to parse date.
          -- It builds a tree of `DiagTreeSlides` from flat map of all slides.
          parseTree :: MaybeT Effect (Tuple DiagTreeSlideId DiagTreeSlides)
          parseTree = do
            Tuple rootSlide flatSlides <- toMaybeT parsedFlat

            let reduce acc { id, isRoot } = if isRoot then snoc acc id else acc
                flatIds = foldl reduce [] flatSlides

            Tuple rootSlide <$> foldM (reduceTree flatSlides) Map.empty flatIds


      tree <- liftEffect $ runMaybeT parseTree

      case tree of
           Just (Tuple rootSlide slides) -> pure { slides, rootSlide }
           Nothing -> throwError $ error dataParseFailMsg

    act $ LoadSlidesSuccess parsed

  act = sendAction appCtx
  dataParseFailMsg = "parsing data failed"

  handleError err =
    if message err == dataParseFailMsg
       then do errLog $ "Parsing data is failed: " <> message err
               act $ LoadSlidesFailure ParsingSlidesDataFailed
       else do errLog $ "Request is failed: " <> message err
               act $ LoadSlidesFailure LoadingSlidesFailed

  handleParseError err = do
    if message err == dataParseFailMsg
       then throwError err -- Raise upper
       else do errLog $ "Parsing data is unexpectedly failed: " <> message err
               act $ LoadSlidesFailure ParsingSlidesDataFailed
               throwError $ error dataParseFailMsg

  reportParseError formatErr = liftEffect $ throwError $ error $
    "Parsing response of loading slides is failed " <>
    "(error: " <> printResponseFormatError formatErr <> ")!"
