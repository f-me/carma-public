module Component.Spinner
     ( spinner
     ) where

import Prelude hiding (div)

import React.DOM (div', div, text)
import React.DOM.Props (className)
import Thermite as T

import App.Store (AppState, AppAction)


spinner :: forall props. T.Render AppState props AppAction
spinner dispatch _ state _ =
  [ div [ className "circle-spinner--with-label" ]
        [ div' [ text $ "Загрузка…" <> show state.currentLocation ]
        , div [ className "circle-spinner--icon" ] []
        ]
  ]
