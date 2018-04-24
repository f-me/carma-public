module Component.App
     ( app
     ) where

import Prelude hiding (div)

import Data.Record.Builder (merge)
import Data.Either (Either (..))

import React (ReactClass, getProps, createElement)
import React.DOM (div)
import React.DOM.Props (className)
import React.Spaces.DOM (h1)
import React.Spaces (renderIn, element, text)

import Utils (storeConnect, createClassStatelessWithSpec)
import Router (Location (..))
import App.Store (AppContext)
import Component.Generic.Spinner (spinner)
import Component.DiagTree.Editor (diagTreeEditor)


appRender
  :: ReactClass { location   :: Location
                , appContext :: AppContext
                }

appRender = f $ \ { appContext, location } -> renderIn wrapper $

  case location of

    DiagTreeEditPartial ->
      element $ editorEl { appContext } []

    NotFound ->
      h1 $ text "Страница не найдена"

    Empty ->
      element $ spinnerEl { withLabel: Left true, appContext } []

  where
    name = "CarmaApp"
    wrapper = div [className name]
    editorEl = createElement diagTreeEditor
    spinnerEl = createElement spinner
    f = createClassStatelessWithSpec specMiddleware

    specMiddleware = _
      { displayName = name
      , shouldComponentUpdate = \this nextProps _ ->
          getProps this <#> _.location <#> (_ /= nextProps.location)
      }


app :: ReactClass { appContext :: AppContext }
app = storeConnect f appRender
  where
    f appState = merge { location: appState.currentLocation }
