module App.Store.DiagTree.Editor.Handlers.LoadSlides
     ( loadSlides
     ) where

import Prelude hiding (id)

import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Eff.Exception (error, message)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)

import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (Tuple), fst, snd)
import Data.Array (snoc)
import Data.Foldable (foldM, foldl)
import Data.StrMap as StrMap
import Data.Map (Map)
import Data.Map as Map
import Data.Foreign (Foreign, unsafeFromForeign)
import Data.JSDate (LOCALE)
import Data.Argonaut.Core as A

import Network.HTTP.Affjax (AJAX, AffjaxResponse, affjax)

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


loadSlides
  :: forall eff
   . AppContext
  -> Aff ( ajax    :: AJAX
         , avar    :: AVAR
         , locale  :: LOCALE
         , console :: CONSOLE
         | eff
         ) Unit

loadSlides appCtx = flip catchError handleError $ do
  (res :: AffjaxResponse Foreign) <- affjax $ getRequest "/_/DiagSlide"

  parsed <- flip catchError handleParseError $ do
    let json = unsafeFromForeign res.response :: A.Json

        -- Parsing single element (called "flat slide" or "backend slide").
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
          isActive <- StrMap.lookup "isActive" x >>= A.toBoolean

          if not isActive
             then pure acc -- Ignore inactive slides
             else parseFlatElem acc x

        parsedFlat =
          A.toArray json
          >>= foldM reduceFlat (Tuple Nothing Map.empty)
          >>= \(Tuple rootSlide flatSlides) ->
                case rootSlide of
                     Just x  -> Just $ Tuple x flatSlides
                     Nothing -> Nothing


        reduceTree
          :: forall reduceTreeEff
           . Map DiagTreeSlideId BackendSlide
          -> DiagTreeSlides
          -> DiagTreeSlideId
          -> MaybeT (Eff (locale :: LOCALE | reduceTreeEff)) DiagTreeSlides

        reduceTree flatSlides acc slideId =
          getSlide flatSlides slideId <#> flip (Map.insert slideId) acc


        -- `Eff` is required to parse date.
        -- It builds a tree of `DiagTreeSlides` from flat map of all slides.
        parseTree :: forall parseTreeEff.
          MaybeT (Eff (locale :: LOCALE | parseTreeEff))
                 (Tuple DiagTreeSlideId DiagTreeSlides)

        parseTree = do
          Tuple rootSlide flatSlides <- toMaybeT parsedFlat

          let reduce acc { id, isRoot } = if isRoot then snoc acc id else acc
              flatIds = foldl reduce [] flatSlides

          Tuple rootSlide <$> foldM (reduceTree flatSlides) Map.empty flatIds


    tree <- liftEff $ runMaybeT parseTree

    case tree of
         Just (Tuple rootSlide slides) -> pure { slides, rootSlide }
         Nothing -> throwError $ error dataParseFailMsg

  act $ LoadSlidesSuccess parsed

  where
    act = sendAction appCtx
    dataParseFailMsg = "parsing data failed"

    handleError err =
      if message err == dataParseFailMsg
         then do errLog $ "Parsing data failed: " <> message err
                 act $ LoadSlidesFailure ParsingSlidesDataFailed
         else do errLog $ "Request failed: " <> message err
                 act $ LoadSlidesFailure LoadingSlidesFailed

    handleParseError err = do
      if message err == dataParseFailMsg
         then throwError err -- Raise upper
         else do errLog $ "Parsing data unexpectedly failed: " <> message err
                 act $ LoadSlidesFailure ParsingSlidesDataFailed
                 throwError $ error dataParseFailMsg
