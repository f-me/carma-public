module App.Store.HandlersSpec
     ( subscribeHandlers
     ) where

import Prelude

import Data.JSDate (LOCALE)

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Aff.AVar (AVAR)

import Network.HTTP.Affjax (AJAX)
import DOM (DOM)

import App.Store (AppContext, subscribe', toStoreListener)
import App.Store.Actions (AppAction (..))
import App.Store.Handlers (appHandler)
import App.Store.DiagTree.Actions (DiagTreeAction (..))
import App.Store.DiagTree.Editor.Handlers (diagTreeEditorHandler)


subscribeHandlers
  :: forall eff
   . AppContext
  -> Aff ( ref     :: REF
         , avar    :: AVAR
         , dom     :: DOM
         , ajax    :: AJAX
         , locale  :: LOCALE
         , now     :: NOW
         , random  :: RANDOM
         , console :: CONSOLE
         | eff
         ) Unit

subscribeHandlers appCtx = do

  runHandler $ \ { prevState, nextState, action } -> case action of
    Navigate _ -> app prevState nextState action
    _ -> ignore

  runHandler $ \ { prevState, nextState, action } -> case action of
    DiagTree (Editor x) ->
      diagTreeEditor (diagTreeEditorLens prevState)
                     (diagTreeEditorLens <$> nextState)
                     x
    _ -> ignore

  where
    ignore = pure unit

    app = appHandler appCtx

    diagTreeEditor = diagTreeEditorHandler appCtx
    diagTreeEditorLens = _.diagTree.editor

    runHandler selector
      = void $ liftEff $ subscribe' appCtx
      $ toStoreListener $ launchAff_ <<< selector
