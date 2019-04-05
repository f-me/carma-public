module ApplicationInit (runApplication) where

import Prelude

import Data.Maybe (Maybe (..))

import Control.Monad.Error.Class (throwError)

import Effect (Effect)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Aff (launchAff_)
import Effect.Exception (message)
import Effect.Console (error)

import Web.HTML (window)
import Web.HTML.Window (document, alert)
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

  store <- createStore reducerThreadFailureHandler appReducer appInitialState
  initRouter $ launchAff_ <<< dispatch store <<< Navigate
  launchAff_ $ subscribeHandlers store
  void $ flip render appDOMEl $ appEl { store }

  where
    appEl = createLeafElement app

    reducerThreadFailureHandler err = do
      error $ "Store reducer thread is failed with exception: " <> message err

      window >>= alert
        "Что-то пошло не так! Настоятельно рекомендуется перезагрузить\
        \ страницу для продолжения нормальной работы системы!"

      throwError err
