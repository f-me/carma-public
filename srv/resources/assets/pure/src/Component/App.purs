module Component.App
     ( app
     ) where

import Prelude hiding (div)

import Data.Either (Either (..))

import Record.Builder (merge)

import React (ReactClass, component, getProps, createLeafElement)
import React.DOM (div, h1', text)
import React.DOM.Props (className)

import Utils (storeConnect)
import Router (Location (..))
import App.Store (AppContext)
import Component.Generic.Spinner (spinner)
import Component.DiagTree.Editor (diagTreeEditor)


appRender
  :: ReactClass { location   :: Location
                , appContext :: AppContext
                }

appRender = defineComponent \ { appContext, location } -> wrapper $ pure $

  case location of

    DiagTreeEditPartial ->
      editorEl { appContext }

    NotFound ->
      h1' $ pure $ text "Страница не найдена"

    Empty ->
      spinnerEl { withLabel: Left true, appContext }

  where
    name = "CarmaApp"
    wrapper = div [className name]
    editorEl = createLeafElement diagTreeEditor
    spinnerEl = createLeafElement spinner

    defineComponent renderFn = component name \this ->
      pure
        { render: renderFn <$> getProps this

        , shouldComponentUpdate: \nextProps _ ->
            getProps this <#> _.location >>> (_ /= nextProps.location)
        }


app :: ReactClass { appContext :: AppContext }
app = storeConnect f appRender where
  f appState = merge { location: appState.currentLocation }
