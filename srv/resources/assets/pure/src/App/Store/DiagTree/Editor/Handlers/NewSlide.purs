module App.Store.DiagTree.Editor.Handlers.NewSlide
     ( newSlide
     ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (error, message, stack)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Maybe.Trans (runMaybeT)

import Data.Maybe (Maybe (..), maybe)
import Data.Foreign (Foreign, unsafeFromForeign)
import Data.Argonaut.Core as A
import Data.Map as Map
import Data.JSDate (LOCALE)

import Network.HTTP.Affjax (AJAX, AffjaxResponse, affjax)

import Utils (toMaybeT)
import Utils.Affjax (getRequest, postRequest)
import App.Store (AppContext)
import App.Store.DiagTree.Editor.Handlers.Helpers (errLog, sendAction)
import App.Store.DiagTree.Editor.Types (DiagTreeSlide (DiagTreeSlide))

import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction (NewSlideSuccess, NewSlideFailure, SelectSlide)
     )

import App.Store.DiagTree.Editor.Handlers.SharedUtils
     ( defaultPartialBackendSlide
     , toBackendSlideFromPartial
     , fromBackendSlide
     , getSlide
     , getBackendSlideId
     )


newSlide
  :: forall eff
   . AppContext
  -> Aff ( avar :: AVAR
         , console :: CONSOLE
         , ajax :: AJAX
         , locale :: LOCALE
         | eff
         ) Unit

newSlide appCtx = flip catchError handleError $ do
  (newSlideRes :: AffjaxResponse Foreign) <-
    affjax $ postRequest "/_/DiagSlide" $
      toBackendSlideFromPartial $ defaultPartialBackendSlide
        { isRoot    = Just true
        , header    = Just "Новый вопрос"
        , body      = Just "?"
        , resources = Just []
        , actions   = Just []
        , answers   = Just []
        }

  slide <- runMaybeT $ do
    let newSlideCreateResult = unsafeFromForeign newSlideRes.response :: A.Json
    newSlideId <- toMaybeT $ getBackendSlideId newSlideCreateResult

    (getSlideRes :: AffjaxResponse Foreign) <-
      liftAff $ affjax $ getRequest $ "/_/DiagSlide/" <> show newSlideId

    let getSlideJson = unsafeFromForeign getSlideRes.response :: A.Json
    x <- toMaybeT $ fromBackendSlide getSlideJson
    y <- liftEff $ runMaybeT $ getSlide (Map.singleton x.id x) x.id
    toMaybeT y

  case slide of
       Nothing -> throwError $ error "Parsing new created slide failed"
       Just x@(DiagTreeSlide { id: newSlideId }) -> do
         act $ NewSlideSuccess x
         act $ SelectSlide [newSlideId]

  where
    act = sendAction appCtx

    reportErr err = errLog $
      "Creating new slide failed: " <> message err
      # \x -> maybe x (\y -> x <> "\nStack trace:\n" <> y) (stack err)

    handleError err = do
      reportErr err
      act NewSlideFailure
