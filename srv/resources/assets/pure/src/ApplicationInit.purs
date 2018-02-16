module ApplicationInit (runApplication) where

import Prelude

import Data.Maybe (Maybe (..))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Control.Monad.Aff (launchAff_)

import DOM.HTML (window) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.HTML.Types (htmlDocumentToDocument) as DOM
import DOM.Node.NonElementParentNode (getElementById) as DOM

import DOM.Node.Types ( Element
                      , ElementId (ElementId)
                      , documentToNonElementParentNode
                      ) as DOM

import React (createElement)
import ReactDOM (render)

import Router (initRouter)
import Component.App (app)

import App.Store (createAppContext, subscribe, dispatch)
import App.Store.Types (StoreEffects)
import App.Store.Actions (AppAction (Navigate))
import App.Store.Reducers (appInitialState, appReducer)
import App.Store.Handlers (appHandler)


runApplication :: forall eff. Eff (StoreEffects eff) Unit
runApplication = do
  (appEl :: DOM.Element) <-
    DOM.window
    >>= DOM.document
    >>= DOM.getElementById (DOM.ElementId "app")
        <<< DOM.documentToNonElementParentNode
        <<< DOM.htmlDocumentToDocument
    >>= case _ of
             Nothing -> unsafeThrow "#app element not found"
             Just el -> pure el

  appCtx <- createAppContext appReducer appInitialState
  initRouter $ launchAff_ <<< dispatch appCtx <<< Navigate
  void $ subscribe appCtx $ appHandler appCtx
  void $ flip render appEl $ createElement app { appContext: appCtx } []
