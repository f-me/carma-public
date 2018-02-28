module App.Store.DiagTree.Editor.Handlers.DeleteSlide
     ( deleteSlide
     ) where

import Prelude

import Control.Monad.Aff (Aff, Milliseconds (Milliseconds), delay)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)

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
  -> Aff (avar :: AVAR, console :: CONSOLE | eff) Unit

deleteSlide appCtx slidePath = do
  errLog "TODO deleting slide isn't implemented yet"
  delay $ Milliseconds 2000.0
  sendAction appCtx $ DeleteSlideFailure slidePath
