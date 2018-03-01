module App.Store.DiagTree.Editor.Handlers.DeleteSlide
     ( deleteSlide
     ) where

import Prelude

import Control.Monad.Aff (Aff, Milliseconds (Milliseconds), delay)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (message, stack)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Parallel (sequential, parallel)

import Data.Maybe (maybe)
import Data.Either (Either (..), either)
import Data.Foreign (Foreign)
import Data.Array (snoc)
import Data.Foldable (foldl, foldM)

import Network.HTTP.Affjax (AJAX, AffjaxResponse, affjax)

import Utils.Affjax (putRequest)
import App.Store (AppContext)
import App.Store.DiagTree.Editor.Types (DiagTreeSlideId)
import App.Store.DiagTree.Editor.Handlers.Helpers (errLog, sendAction)

import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction (DeleteSlideFailure)
     )


deleteSlide
  :: forall eff
   . AppContext
  -> Array DiagTreeSlideId
  -> Aff (avar :: AVAR, console :: CONSOLE, ajax :: AJAX | eff) Unit

deleteSlide appCtx slidePath = flip catchError handleError $ do
  -- We're about to remove an answer from parent slide
  -- and to deactivate slide we're deleting.

  (_ :: Array (AffjaxResponse Foreign)) <-
    (extractParResult =<< _) $
      sequential $ foldl (\acc x -> snoc <$> acc <*> x) (pure [])
        [ parallel $ catchPar $ affjax $ putRequest "TODO"
        , parallel $ catchPar $ affjax $ putRequest "TODO"
        ]

  errLog "TODO deleting slide isn't implemented yet"
  delay $ Milliseconds 2000.0
  act $ DeleteSlideFailure slidePath

  where
    act = sendAction appCtx
    catchPar m = catchError (Right <$> m) (Left >>> pure)

    extractParResult =
      foldM (\acc x -> x # either throwError pure <#> snoc acc) []

    reportErr err = errLog $
      "Deleting slide (" <> show slidePath <> ") failed: " <> message err
      # \x -> maybe x (\trace -> x <> "\nStack trace:\n" <> trace) (stack err)

    handleError err = do
      reportErr err
      act $ DeleteSlideFailure slidePath
