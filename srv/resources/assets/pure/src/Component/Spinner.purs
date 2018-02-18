module Component.Spinner
     ( spinner
     ) where

import Prelude hiding (div)

import React (ReactClass)
import React.DOM (div', div, text)
import React.DOM.Props (className)

import Utils (createClassStatelessWithSpec)
import App.Store (AppContext)


spinnerRender
  :: ReactClass { withLabel  :: Boolean
                , appContext :: AppContext
                }

spinnerRender = createClassStatelessWithSpec specMiddleware $ \props ->
  if props.withLabel

     then div
          [ className "circle-spinner--with-label" ]
          [ div' [ text $ "Загрузка…" ]
          , div [ className "circle-spinner--icon" ] []
          ]

     else div
          [ className "circle-spinner--icon" ]
          []

  where
    specMiddleware = _
      { displayName = "Spinner"
      , shouldComponentUpdate = \_ _ _ -> pure false
      }


spinner :: ReactClass { withLabel :: Boolean, appContext :: AppContext }
spinner = spinnerRender
