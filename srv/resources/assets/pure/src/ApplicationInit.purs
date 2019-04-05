module ApplicationInit (runApplication) where

import Prelude

import Data.Maybe (Maybe (..))

import Effect (Effect)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Aff (launchAff_)

import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toDocument)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM (Element)

import React (createLeafElement)
import ReactDOM (render)

import Router (initRouter)
import Component.App (app)

import App.Store (createStore, dispatch)
import App.Store.Actions (AppAction (Navigate))
import App.Store.Reducers (appInitialState, appReducer)
import App.Store.HandlersSpec (subscribeHandlers)


runApplication :: Effect Unit
runApplication = do
  (appDOMEl :: Element) <-
    window
    >>= document
    >>= getElementById "app"
        <<< toNonElementParentNode
        <<< toDocument
    >>= case _ of
             Nothing -> unsafeThrow "#app element not found"
             Just el -> pure el

  appContext <- createStore appReducer appInitialState
  initRouter $ launchAff_ <<< dispatch appContext <<< Navigate
  launchAff_ $ subscribeHandlers appContext
  void $ flip render appDOMEl $ appEl { appContext }

  where appEl = createLeafElement app
