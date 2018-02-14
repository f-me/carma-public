module Component.Spinner
     ( spinner
     ) where

import Prelude hiding (div)

import Data.Record.Builder (merge)

import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Console (log)

import React (ReactClass, createClassStateless)
import React.DOM (div', div, text)
import React.DOM.Props (className)

import App.Store (AppContext, AppState, AppAction)
import Router (Location)
import Utils (StoreConnectEff, storeConnect)


spinnerRender
  :: forall eff
   . ReactClass { spFoo      :: String
                , spBar      :: Location
                , spBaz      :: String
                , appContext :: AppContext (StoreConnectEff eff)
                }

spinnerRender = createClassStateless $ \props ->
  let foo = unsafePerformEff $ log "spinner render called"
  in div
  [ className "circle-spinner--with-label" ]
  [ div' [ text $ "Загрузка…"
         , text props.spFoo
         , text " | "
         , text $ show props.spBar
         , text " | "
         , text props.spBaz
         ]
  , div [ className "circle-spinner--icon" ] []
  ]


spinner
  :: forall eff
   . ReactClass { spBaz      :: String
                , appContext :: AppContext (StoreConnectEff eff)
                }

spinner = storeConnect f spinnerRender
  where
    f appState = merge { spFoo: "spinner foo"
                       , spBar: appState.currentLocation
                       }
