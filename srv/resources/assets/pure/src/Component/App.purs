module Component.App
     ( app
     ) where

import Prelude hiding (div)

import Data.Monoid (mempty)
import Data.Either (Either (..))
import Data.Record.Builder (merge)

import React (ReactClass, getProps, createElement)
import React.DOM (div, h1, text)
import React.DOM.Props (className)

import Utils (storeConnect, createClassStatelessWithSpec)
import Router (Location (..))
import App.Store (AppContext)
import Component.Generic.Spinner (spinner)
import Component.DiagTree.Editor (diagTreeEditor)


appRender
  :: ReactClass { location   :: Location
                , appContext :: AppContext
                }

appRender = f $ \ { appContext, location } -> wrapper $ pure $

  case location of

    DiagTreeEditPartial ->
      editorEl { appContext } mempty

    NotFound ->
      h1 mempty $ pure $ text "Страница не найдена"

    Empty ->
      spinnerEl { withLabel: Left true, appContext } mempty

  where
    name = "CarmaApp"
    wrapper = div [className name]
    editorEl = createElement diagTreeEditor
    spinnerEl = createElement spinner
    f = createClassStatelessWithSpec specMiddleware

    specMiddleware = _
      { displayName = name
      , shouldComponentUpdate = \this nextProps _ ->
          getProps this <#> _.location >>> (_ /= nextProps.location)
      }


app :: ReactClass { appContext :: AppContext }
app = storeConnect f appRender
  where
    f appState = merge { location: appState.currentLocation }
