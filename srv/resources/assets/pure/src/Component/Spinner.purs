module Component.Spinner
     ( spinner
     ) where

import Prelude hiding (div)

import React (ReactClass)
import React.DOM (div', div, text)
import React.DOM.Props (className)

import Utils (createClassStatelessWithSpec)
import App.Store (AppContext)
import App.Store.Types (StoreConnectEffects)


spinnerRender
  :: forall eff
   . ReactClass { appContext :: AppContext (StoreConnectEffects eff) }

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


spinner
  :: forall eff
   . ReactClass { appContext :: AppContext (StoreConnectEffects eff) }

spinner = spinnerRender
