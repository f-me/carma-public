module Component.App
     ( app
     ) where

import Prelude

import Data.Record.Builder (merge)

import React (ReactClass, getProps, createElement)
import React.DOM (div', h1', text)

import Utils (StoreConnectEff, storeConnect, createClassStatelessWithSpec)
import Router (Location (..))
import App.Store (AppContext)
import Component.Spinner (spinner)
import Component.DiagTree.Editor (diagTreeEditor)


appRender
  :: forall eff
   . ReactClass { location   :: Location
                , appContext :: AppContext (StoreConnectEff eff)
                }

appRender = createClassStatelessWithSpec specMiddleware $ \props -> div' $

  case props.location of

    DiagTreeEditPartial ->
      [ createElement diagTreeEditor { appContext : props.appContext } []
      ]

    NotFound ->
      [ h1' [text "Страница не найдена"]
      ]

    Empty ->
      [ createElement spinner { appContext : props.appContext } []
      ]

  where
    specMiddleware = _
      { displayName = "App"
      , shouldComponentUpdate = \this nextProps _ ->
          getProps this <#> _.location <#> (_ /= nextProps.location)
      }


app
  :: forall eff
   . ReactClass { appContext :: AppContext (StoreConnectEff eff) }

app = storeConnect f appRender
  where
    f appState = merge { location: appState.currentLocation }
