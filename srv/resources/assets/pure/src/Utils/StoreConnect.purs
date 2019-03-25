module Utils.StoreConnect
     ( StoreSelector
     , storeConnect
     ) where

import Prelude

import Data.Maybe (Maybe (..), fromMaybe)
import Record.Builder (Builder, build)

import React
     ( ReactClass
     , component, unsafeCreateLeafElement
     , modifyState, getState, getProps
     )

-- local imports

import App.Store.Reducers (AppState)

import App.Store
     ( AppContext, toStoreListener, subscribe, unsubscribe, getAppState
     )


type StoreSelector props1 props2
   = AppState
  -> Builder
       { appContext :: AppContext | props1 }
       { appContext :: AppContext | props2 }


storeConnect
  :: forall props1 props2
   . StoreSelector props1 props2
  -> ReactClass { appContext :: AppContext | props2 }
  -> ReactClass { appContext :: AppContext | props1 }

storeConnect storeSelector child = component "StoreConnect" spec where
  childEl = unsafeCreateLeafElement child
  applyProps = _.mappedProps >>> childEl
  renderFn this = getState this <#> applyProps

  getMappedProps props =
    getAppState props.appContext <#>
      \appState -> build (storeSelector appState) props

  spec this = getProps this >>= getMappedProps <#> \mappedProps ->
    { state: { subscription: Nothing, mappedProps }
    , render: renderFn this

    , unsafeComponentWillMount: do
        let listener = toStoreListener \{ prevState, nextState } -> do
              let appState = fromMaybe prevState nextState
              x <- getProps this <#> build (storeSelector appState)
              modifyState this _ { mappedProps = x }

        { appContext } <- getProps this
        subscription   <- subscribe appContext listener
        modifyState this _ { subscription = Just subscription }

    , unsafeComponentWillReceiveProps: \nextProps -> do
        x <- getMappedProps nextProps
        modifyState this _ { mappedProps = x }

    , componentWillUnmount: do
        { appContext }   <- getProps this
        { subscription } <- getState this

        case subscription of
             Nothing -> pure unit
             Just x  -> unsubscribe appContext x
    }
