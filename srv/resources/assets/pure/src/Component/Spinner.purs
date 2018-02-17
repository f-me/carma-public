module Component.Spinner
     ( spinner
     ) where

import Prelude hiding (div)

import React (ReactClass)
import React.DOM (div', div, text)
import React.DOM.Props (className)

import Utils (createClassStatelessWithSpec)
import App.Store (AppContext)


spinnerRender :: ReactClass { appContext :: AppContext }
spinnerRender = createClassStatelessWithSpec specMiddleware $ const $ div
  [ className "circle-spinner--with-label" ]
  [ div' [ text $ "Загрузка…" ]
  , div [ className "circle-spinner--icon" ] []
  ]

  where
    specMiddleware = _
      { displayName = "Spinner"
      , shouldComponentUpdate = \_ _ _ -> pure false
      }


spinner :: ReactClass { appContext :: AppContext }
spinner = spinnerRender
