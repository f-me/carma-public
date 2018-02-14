module Component.Spinner
     ( spinner
     ) where

import Prelude hiding (div)

import React (ReactClass, createClassStateless)
import React.DOM (div', div, text)
import React.DOM.Props (className)

import Utils (StoreConnectEff)
import App.Store (AppContext)


spinnerRender
  :: forall eff
   . ReactClass { appContext :: AppContext (StoreConnectEff eff) }

spinnerRender = createClassStateless $ \props -> div
  [ className "circle-spinner--with-label" ]
  [ div' [ text $ "Загрузка…" ]
  , div [ className "circle-spinner--icon" ] []
  ]


spinner
  :: forall eff
   . ReactClass { appContext :: AppContext (StoreConnectEff eff) }

spinner = spinnerRender
