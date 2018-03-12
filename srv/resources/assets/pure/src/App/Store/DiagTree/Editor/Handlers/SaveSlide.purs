module App.Store.DiagTree.Editor.Handlers.SaveSlide
     ( saveSlide
     ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (message, stack)
import Control.Monad.Error.Class (catchError)

import Data.Maybe (Maybe, maybe)

import Network.HTTP.Affjax (AJAX)

import App.Store (AppContext)
import App.Store.DiagTree.Editor.Handlers.Helpers (errLog, sendAction)

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlideId
     , DiagTreeSlide
     , DiagTreeSlideAttachment
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

saveSlide appCtx slidePath slide newAnswers = flip catchError handleError $ do

  errLog "TODO implement saving slide changes"
  act $ SaveSlideSuccess slidePath
  act LoadSlidesRequest -- Reloading updated slides

  where
    act = sendAction appCtx

    reportErr err = errLog $
      "Saving slide (" <> show slidePath <> ") failed: " <> message err
      # \x -> maybe x (\y -> x <> "\nStack trace:\n" <> y) (stack err)

    handleError err = do
      reportErr err
      act $ SaveSlideFailure slidePath
