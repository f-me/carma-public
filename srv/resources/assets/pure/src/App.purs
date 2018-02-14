module App (runApp) where

import Prelude

import Data.Maybe (Maybe (..))
import Data.Record.Builder (merge)

import Control.Monad.Eff (Eff)
import Control.Monad.Aff (liftEff')
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Trans.Class (lift)
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

import React (ReactClass, createClassStateless, createClass, spec, getProps, createElement)
import ReactDOM (render)
import React.DOM (div', h1', h2', text, button)
import React.DOM.Props (onClick)

import Utils (StoreConnectEff, storeConnect)
import Router (Location (..), initRouter, navigateToRoute)
import Component.Spinner (spinner)

import App.Store ( AppContext
                 , AppState
                 , AppAction (..)
                 , createAppContext
                 , dispatch
                 , subscribe
                 )


appRender
  :: forall eff. ReactClass { appFoo     :: Location
                            , appContext :: AppContext (StoreConnectEff eff)
                            }
appRender =
  {-- createClassStateless $ \props -> div' --}

  createClass $ spec unit renderFn # _
    { shouldComponentUpdate = \_ _ _ -> pure false
    }

  where
    renderFn this = do
      log "app render called"
      props <- getProps this
      case props.appFoo of
           {-- DiagTreeEditPartial -> --}
           _ -> pure $ div'
                [ h1' [text $ "Loading…" <> show props.appFoo]
                , createElement spinner
                                { spBaz      : "spinner BAZ from app"
                                , appContext : props.appContext
                                }
                                []
                ]

  {--[ h1' [ text $ show state.currentLocation ]
  , h2' [ text "some testing text" ]
  , button [ onClick (const $ dispatch $ Navigate DiagTreeEditPartial) ]
           [ text "just do it!" ]
  ]--}

app
  :: forall eff . ReactClass { appContext :: AppContext (StoreConnectEff eff) }
app = storeConnect f appRender
  where
    f appState = merge { appFoo: appState.currentLocation }


runApp
  :: forall eff
   . Eff ( StoreConnectEff ( console :: CONSOLE
                           , dom :: DOM
                           , ref :: REF
                           | eff
                           )
         ) Unit
runApp = do
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

  -- Convention about passing `appContext` property:
  --   1. Every component must require to receive `appContext` prop;
  --   2. Every component must pass `appContext` prop to every child component.
  -- This will help to connect to store at any level of components tree without
  -- changing anything else but that component.
  void $ render (createElement app { appContext: appCtx } []) appEl

  where

    appInitialState :: AppState
    appInitialState = { currentLocation: Empty
                      }

    storeReducer state (Navigate route) =
      if state.currentLocation /= route
         then Just $ state {currentLocation = route}
         else Nothing
