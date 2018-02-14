module ApplicationInit (runApplication) where

import Prelude

import Data.Maybe (Maybe (..))

import Control.Monad.Eff (Eff)
import Control.Monad.Aff (liftEff')
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)

import DOM (DOM)
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

import Utils (StoreConnectEff)
import Router (Location (..), initRouter, navigateToRoute)
import Component.App (app)

import App.Store ( AppState
                 , AppAction (..)
                 , createAppContext
                 , dispatch
                 , subscribe
                 )


runApplication
  :: forall eff
   . Eff ( StoreConnectEff ( console :: CONSOLE
                           , dom :: DOM
                           , ref :: REF
                           | eff
                           )
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

  appCtx <- createAppContext storeReducer appInitialState
  initRouter $ dispatch appCtx <<< Navigate

  void $ subscribe appCtx $ const $ case _ of
    Navigate route -> liftEff' $ navigateToRoute route
    _              -> pure unit

  void $ render (createElement app { appContext: appCtx } []) appEl

  where

    appInitialState :: AppState
    appInitialState = { currentLocation: Empty
                      }

    storeReducer state (Navigate route) =
      if state.currentLocation /= route
         then Just $ state {currentLocation = route}
         else Nothing
