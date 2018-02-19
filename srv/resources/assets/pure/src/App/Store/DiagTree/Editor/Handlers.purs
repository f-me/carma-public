module App.Store.DiagTree.Editor.Handlers
     ( diagTreeEditorHandler
     ) where

import Prelude hiding (id)

import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Eff.Exception (error, message)
import Control.Monad.Eff.Console as Log
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Control.Monad.Trans.Class (lift)

import Data.Int (fromNumber)
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Tuple (Tuple (Tuple), fst, snd)
import Data.Array (snoc)
import Data.Foldable (foldM, foldl)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Map (Map)
import Data.Map as Map
import Data.HTTP.Method (Method (GET))
import Data.MediaType.Common (applicationJSON)
import Data.Foreign (Foreign, unsafeFromForeign)
import Data.JSDate (LOCALE, parse, toDateTime)

import Data.Argonaut.Core
     ( Json
     , toArray, toBoolean, toNumber, toObject, toString
     )

import Network.HTTP.Affjax (AJAX, AffjaxResponse, affjax, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader (..))

import Utils (toMaybeT)
import App.Store (AppContext, dispatch)
import App.Store.DiagTree.Editor.Reducers (DiagTreeEditorState)

import App.Store.Actions (AppAction (DiagTree))
import App.Store.DiagTree.Actions (DiagTreeAction (Editor))

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlides
     , DiagTreeSlideId
     , DiagTreeSlide (DiagTreeSlide)
     )

import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction (..)
     , LoadSlidesFailureReason (..)
     )


diagTreeEditorHandler
  :: forall eff
   . AppContext
  -> DiagTreeEditorState
  -> DiagTreeEditorAction
  -> Aff ( ajax    :: AJAX
         , avar    :: AVAR
         , locale  :: LOCALE
         , console :: Log.CONSOLE
         | eff
         ) Unit

diagTreeEditorHandler appCtx state action = case action of
  LoadSlidesRequest -> loadSlides appCtx
  _ -> pure unit


loadSlides
  :: forall eff
   . AppContext
  -> Aff ( ajax    :: AJAX
         , avar    :: AVAR
         , locale  :: LOCALE
         , console :: Log.CONSOLE
         | eff
         ) Unit

loadSlides appCtx = flip catchError handleError $ do
  (res :: AffjaxResponse Foreign) <-
    affjax $ defaultRequest
      { url     = "/_/DiagSlide"
      , method  = Left GET
      , headers = [ Accept      applicationJSON
                  , ContentType applicationJSON
                  ]
      }

  parsed <- flip catchError handleParseError $ do
    let json = unsafeFromForeign res.response :: Json

        -- Parsing single element
        -- (not `DiagTreeSlide` yet, just a record of a slide for flat map).
        parseFlatElem acc x = do
          id     <- StrMap.lookup "id"     x >>= toNumber >>= fromNumber
          isRoot <- StrMap.lookup "isRoot" x >>= toBoolean
          header <- StrMap.lookup "header" x >>= toString
          body   <- StrMap.lookup "body"   x >>= toString
          ctime  <- StrMap.lookup "ctime"  x >>= toString

          let newAcc = if isRoot && fst acc == Nothing
                          then Tuple (Just id) $ snd acc
                          else acc

          pure $ flip map newAcc $ Map.insert id
            { id, isRoot, ctime, header, body }


        -- Building flat slides map
        reduceFlat acc jsonItem = do
          x        <- toObject jsonItem
          isActive <- StrMap.lookup "isActive" x >>= toBoolean

          if not isActive
             then pure acc -- Ignore inactive slides
             else parseFlatElem acc x

        parsedFlat =
          toArray json
          >>= foldM reduceFlat (Tuple Nothing Map.empty)
          >>= \(Tuple rootSlide flatSlides) ->
                case rootSlide of
                     Just x  -> Just $ Tuple x flatSlides
                     Nothing -> Nothing


        reduceTree
          :: forall eff props
           . Map DiagTreeSlideId FlatSlide
          -> DiagTreeSlides
          -> DiagTreeSlideId
          -> MaybeT (Eff (locale :: LOCALE | eff)) DiagTreeSlides

        reduceTree flatSlides acc slideId = do
          { id, isRoot, ctime, header, body } <-
            toMaybeT $ slideId `Map.lookup` flatSlides

          parsedCtime <- do
            jsDate <- liftEff $ parse ctime
            toMaybeT $ toDateTime jsDate

          toMaybeT $ pure $ flip (Map.insert id) acc $
            DiagTreeSlide
              { id
              , isRoot
              , ctime: parsedCtime
              , header
              , body
              , resources: []
              , answers: []
              , actions: []
              }


        -- `Eff` is required to parse date.
        -- It builds a tree of `DiagTreeSlides` from flat map of all slides.
        parseTree :: forall eff.
          MaybeT (Eff (locale :: LOCALE | eff))
                 (Tuple DiagTreeSlideId DiagTreeSlides)

        parseTree = do
          Tuple rootSlide flatSlides <- toMaybeT parsedFlat

          let reduce acc { id, isRoot } = if isRoot then snoc acc id else acc
              flatIds = foldl reduce [] flatSlides

          slidesTree <- foldM (reduceTree flatSlides) Map.empty flatIds
          pure $ Tuple rootSlide slidesTree


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


errLog :: forall eff. String -> Aff (console :: Log.CONSOLE | eff) Unit
errLog = liftEff <<< Log.error <<< ("Diag Tree Editor: " <> _)

sendAction :: forall eff.
  AppContext -> DiagTreeEditorAction -> Aff (avar :: AVAR | eff) Unit

sendAction appCtx = dispatch appCtx <<< DiagTree <<< Editor

type FlatSlide =
  { id     :: DiagTreeSlideId
  , isRoot :: Boolean
  , ctime  :: String
  , header :: String
  , body   :: String
  }
