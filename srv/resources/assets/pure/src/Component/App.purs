module Component.App
     ( app
     ) where

import Prelude

import Data.Record.Builder (merge)

import React (ReactClass, getProps)
import React.DOM (IsDynamic (IsDynamic), mkDOM)
import React.Spaces.DOM (h1)
import React.Spaces ((^), renderIn, text)

import Utils (storeConnect, createClassStatelessWithSpec)
import Router (Location (..))
import App.Store (AppContext)
import Component.Spinner (spinner)
import Component.DiagTree.Editor (diagTreeEditor)


appRender
  :: ReactClass { location   :: Location
                , appContext :: AppContext
                }

appRender = f $ \ { appContext, location } -> renderIn wrapper $

  case location of

    DiagTreeEditPartial ->
      diagTreeEditor ^ { appContext }

    NotFound ->
      h1 $ text "Страница не найдена"

    Empty ->
      spinner ^ { withLabel: true
                , appContext
                }

  where
    name = "carma-app"
    wrapper = mkDOM (IsDynamic false) name []
    f = createClassStatelessWithSpec specMiddleware

    specMiddleware = _
      { displayName = "App"
      , shouldComponentUpdate = \this nextProps _ ->
          getProps this <#> _.location <#> (_ /= nextProps.location)
      }


app :: ReactClass { appContext :: AppContext }
app = storeConnect f appRender
  where
    f appState = merge { location: appState.currentLocation }
