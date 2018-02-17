module ApplicationInit (runApplication) where

import Prelude

import Data.Maybe (Maybe (..))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Control.Monad.Aff (launchAff_, forkAff)
import Control.Monad.Aff.AVar (AVAR)

import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.HTML.Types (htmlDocumentToDocument) as DOM
import DOM.Node.NonElementParentNode (getElementById) as DOM

import DOM.Node.Types
     ( Element
     , ElementId (ElementId)
     , documentToNonElementParentNode
     ) as DOM

import React (createElement)
import ReactDOM (render)

import Router (initRouter)
import Component.App (app)

import App.Store (createAppContext, reduceLoop, dispatch)
import App.Store.Actions (AppAction (Navigate))
import App.Store.Reducers (appInitialState, appReducer)
import App.Store.HandlersSpec (subscribeHandlers)


runApplication :: Eff ( ref     :: REF
                      , avar    :: AVAR
                      , dom     :: DOM
                      , console :: CONSOLE
                      ) Unit

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

  launchAff_ $ do
    appCtx <- createAppContext appInitialState
    void $ forkAff $ reduceLoop appCtx appReducer
    liftEff $ initRouter $ launchAff_ <<< dispatch appCtx <<< Navigate
    subscribeHandlers appCtx

    liftEff $ void $
      flip render appEl $ createElement app { appContext: appCtx } []
