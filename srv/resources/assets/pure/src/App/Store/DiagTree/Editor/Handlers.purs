module App.Store.DiagTree.Editor.Handlers
     ( diagTreeEditorHandler
     ) where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Eff.Exception (error, message)
import Control.Monad.Eff.Console (CONSOLE, error) as Log
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)

import Data.Int (fromNumber)
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Tuple (Tuple (Tuple), fst, snd)
import Data.Array (snoc)
import Data.Foldable (foldM)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Map as Map
import Data.HTTP.Method (Method (GET))
import Data.MediaType.Common (applicationJSON)
import Data.Foreign (Foreign, unsafeFromForeign)
import Data.Argonaut.Core (Json, toArray, toBoolean, toNumber, toObject)

import Network.HTTP.Affjax (AJAX, AffjaxResponse, affjax, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader (..))

import App.Store (AppContext, dispatch)
import App.Store.DiagTree.Editor.Reducers (DiagTreeEditorState)

import App.Store.Actions (AppAction (DiagTree))
import App.Store.DiagTree.Actions (DiagTreeAction (Editor))
import App.Store.DiagTree.Editor.Types (DiagTreeSlides, DiagTreeSlideId)

import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction (..)
     , LoadSlidesFailureReason (..)
     )


diagTreeEditorHandler
  :: forall eff
   . AppContext
  -> DiagTreeEditorState
  -> DiagTreeEditorAction
  -> Aff (ajax :: AJAX, console :: Log.CONSOLE, avar :: AVAR | eff) Unit

diagTreeEditorHandler appCtx state action = case action of
  LoadSlidesRequest -> loadSlides appCtx
  _ -> pure unit


loadSlides
  :: forall eff
   . AppContext
  -> Aff (ajax :: AJAX, console :: Log.CONSOLE, avar :: AVAR | eff) Unit

loadSlides appCtx = flip catchError handleError $ do
  (res :: AffjaxResponse Foreign) <-
    affjax $ defaultRequest
      { url = "/_/DiagSlide"
      , method = Left GET
      , headers = [ Accept      applicationJSON
                  , ContentType applicationJSON
                  ]
      }

  parsed <- flip catchError handleParseError $ do
    let json = unsafeFromForeign res.response :: Json

        itemF :: Array (StrMap Json) -> Json -> Maybe (Array (StrMap Json))
        itemF acc x = toObject x <#> snoc acc

        itemFieldsF
          :: Tuple (Maybe DiagTreeSlideId) DiagTreeSlides
          -> StrMap Json
          -> Maybe (Tuple (Maybe DiagTreeSlideId) DiagTreeSlides)

        itemFieldsF acc x = do
          idVal <- StrMap.lookup "id" x `joinMap` toNumber `joinMap` fromNumber
          isRoot <- StrMap.lookup "isRoot" x `joinMap` toBoolean
          isActive <- StrMap.lookup "isActive" x `joinMap` toBoolean

          if not isActive
             then pure acc -- Ignore inactive slides
             else do
               let newAcc =
                     if isRoot && fst acc == Nothing
                        then Tuple (Just idVal) $ snd acc
                        else acc

               pure $ flip map newAcc $
                 Map.insert idVal { id: idVal, isRoot: isRoot }

        parsedResult = toArray json
          `joinMap` foldM itemF []
          `joinMap` foldM itemFieldsF (Tuple Nothing Map.empty)

    case parsedResult of

         Just (Tuple (Just rootSlide) slides) -> pure $
           { slides    : slides
           , rootSlide : rootSlide
           }

         _ -> throwError $ error dataParseFailMsg

  act $ LoadSlidesSuccess parsed

  where
    dataParseFailMsg = "parsing data failed"
    errLog = liftEff <<< Log.error <<< ("Diag Tree Editor: " <> _)
    act = dispatch appCtx <<< DiagTree <<< Editor

    joinMap :: forall a b. Maybe a -> (a -> Maybe b) -> Maybe b
    joinMap a b = join $ a <#> b

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
