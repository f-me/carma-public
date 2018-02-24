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

import Data.Int (fromNumber)
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Tuple (Tuple (Tuple), fst, snd)
import Data.Array (snoc)
import Data.Foldable (foldM, foldl)
import Data.StrMap as StrMap
import Data.Map (Map)
import Data.Map as Map
import Data.HTTP.Method (Method (GET))
import Data.MediaType.Common (applicationJSON)
import Data.Foreign (Foreign, unsafeFromForeign)
import Data.JSDate (LOCALE, parse, toDateTime)

import Data.Argonaut.Core
     ( Json
     , toArray, toObject, isNull, toBoolean, toNumber, toString
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

        reduceResource acc jsonItem = do
          x    <- toObject jsonItem
          text <- StrMap.lookup "text" x >>= toString
          file <- StrMap.lookup "file" x >>= toString
          pure $ snoc acc { text, file }

        reduceAction acc jsonItem = do
          x       <- toObject jsonItem
          label   <- StrMap.lookup "label" x >>= toString
          service <- StrMap.lookup "svc"   x >>= toString
          pure $ snoc acc { label, service }

        reduceAnswer acc jsonItem = do
          x <- toObject jsonItem

          nextSlide <- StrMap.lookup "nextSlide" x >>= toNumber >>= fromNumber
          header    <- StrMap.lookup "header"    x >>= toString
          text      <- StrMap.lookup "text"      x >>= toString

          file <-
            case StrMap.lookup "file" x of
                 Nothing  -> pure Nothing  -- Field is not set (that's okay)
                 Just raw -> if isNull raw -- Field is set to `null`
                                then pure Nothing
                                else toString raw <#> Just

          pure $ snoc acc { nextSlide, header, text, file }

        -- Parsing single element
        -- (not `DiagTreeSlide` yet, just a record of a slide for flat map).
        parseFlatElem acc x = do
          id        <- StrMap.lookup "id"        x >>= toNumber >>= fromNumber
          isRoot    <- StrMap.lookup "isRoot"    x >>= toBoolean
          ctime     <- StrMap.lookup "ctime"     x >>= toString
          header    <- StrMap.lookup "header"    x >>= toString
          body      <- StrMap.lookup "body"      x >>= toString

          resources <- StrMap.lookup "resources" x >>= toArray
                                                   >>= foldM reduceResource []

          answers   <- StrMap.lookup "answers"   x >>= toArray
                                                   >>= foldM reduceAnswer []

          actions   <- StrMap.lookup "actions"   x >>= toArray
                                                   >>= foldM reduceAction []

          let newAcc = if isRoot && fst acc == Nothing
                          then Tuple (Just id) $ snd acc
                          else acc

          pure $ flip map newAcc $ Map.insert id
            { id, isRoot, ctime, header, body, resources, answers, actions }


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


        getSlide
          :: forall getSlideEff
           . Map DiagTreeSlideId FlatSlide
          -> DiagTreeSlideId
          -> MaybeT (Eff (locale :: LOCALE | getSlideEff)) DiagTreeSlide

        getSlide flatSlides slideId = do
          { id, isRoot, ctime, header, body, resources, answers, actions } <-
            toMaybeT $ slideId `Map.lookup` flatSlides

          parsedCtime <- do
            jsDate <- liftEff $ parse ctime
            toMaybeT $ toDateTime jsDate

          slideAnswers <- foldM answerReducer Map.empty answers

          toMaybeT $ pure $
            DiagTreeSlide
              { id
              , isRoot
              , ctime: parsedCtime
              , header
              , body
              , resources
              , answers: slideAnswers
              , actions
              }

          where
            answerReducer acc { nextSlide, header, text, file } =
              getSlide flatSlides nextSlide <#> \slide ->
                let x = { nextSlide: slide, header, text, file }
                 in Map.insert nextSlide x acc

        reduceTree
          :: forall reduceTreeEff
           . Map DiagTreeSlideId FlatSlide
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


errLog :: forall eff. String -> Aff (console :: Log.CONSOLE | eff) Unit
errLog = liftEff <<< Log.error <<< ("Diag Tree Editor: " <> _)

sendAction :: forall eff.
  AppContext -> DiagTreeEditorAction -> Aff (avar :: AVAR | eff) Unit

sendAction appCtx = dispatch appCtx <<< DiagTree <<< Editor

type FlatSlide =
  { id        :: DiagTreeSlideId
  , isRoot    :: Boolean
  , ctime     :: String
  , header    :: String
  , body      :: String
  , resources :: Array { text  :: String, file    :: String }
  , actions   :: Array { label :: String, service :: String }

  , answers   :: Array { nextSlide :: DiagTreeSlideId
                       , header    :: String
                       , text      :: String
                       , file      :: Maybe String -- could be not set or `null`
                       }
  }
