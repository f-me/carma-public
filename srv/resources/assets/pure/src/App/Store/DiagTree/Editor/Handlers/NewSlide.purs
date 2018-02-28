module App.Store.DiagTree.Editor.Handlers.NewSlide
     ( newSlide
     ) where

import Prelude

import Control.Monad.Aff (Aff, Milliseconds (Milliseconds), delay)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)

import App.Store (AppContext)
import App.Store.DiagTree.Editor.Handlers.Helpers (errLog, sendAction)

import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction (NewSlideFailure)
     )


newSlide
  :: forall eff
   . AppContext
  -> Aff (avar :: AVAR, console :: CONSOLE | eff) Unit

newSlide appCtx = do
  errLog "TODO new slide isn't implemented yet"
  delay $ Milliseconds 2000.0
  sendAction appCtx NewSlideFailure
