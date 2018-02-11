module App (runApp) where

import Prelude

import Data.Maybe (Maybe (..))

import Control.Monad.Eff (Eff)
import Control.Monad.Aff (liftEff')
import Control.Monad.Eff.Console (CONSOLE)
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

import React (createElement) as React
import ReactDOM (render) as React
import React.DOM (h1', h2', text, button)
import React.DOM.Props (onClick)

import Thermite as T

import Utils (createReactClass)
import Router (Location (..), initRouter, navigateToRoute)


type AppState =
  { currentLocation :: Location
  }

data AppAction
  = Navigate Location


appRender :: forall props. T.Render AppState props AppAction
appRender dispatch _ state _ =
  [ h1' [ text $ show state.currentLocation ]
  , h2' [ text "some testing text" ]
  , button [ onClick (const $ dispatch $ Navigate DiagTreeEdit) ]
           [ text "just do it!" ]
  ]


runApp :: Eff ( console :: CONSOLE
              , dom :: DOM
              , dom :: DOM
              , ref :: REF
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

  let component =
        createReactClass appSpec appInitialState \spec dispatcher ->
          spec { displayName = "App"

               , componentDidMount = \this ->
                   initRouter $ dispatcher this <<< Navigate

               , componentWillUnmount = \_ ->
                   unsafeThrow "<App> component isn't supposed to be unmounted"
               }

  void $ React.render (React.createElement component unit []) appEl

  where appInitialState :: AppState
        appInitialState = { currentLocation: Empty
                          }

        appActionHandler (Navigate route) _ state =
          when (state.currentLocation /= route) $ do
            _ <- T.modifyState _ {currentLocation = route}
            lift $ liftEff' $ navigateToRoute route

        appSpec
          :: T.Spec ( console :: CONSOLE
                    , dom :: DOM
                    , ref :: REF
                    ) AppState Unit AppAction
        appSpec
          = T.simpleSpec appActionHandler appRender
