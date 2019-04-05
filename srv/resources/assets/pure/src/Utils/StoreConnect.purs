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

import App.Store (Store, subscribe, unsubscribe, getStoreState)


type StoreSelector props1 props2 state action
   = state
  -> Builder
       { store :: Store state action | props1 }
       { store :: Store state action | props2 }


storeConnect
  :: forall props1 props2 state action
   . StoreSelector props1 props2 state action
  -> ReactClass { store :: Store state action | props2 }
  -> ReactClass { store :: Store state action | props1 }

storeConnect storeSelector child = component "StoreConnect" spec where
  childEl = unsafeCreateLeafElement child
  applyProps = _.mappedProps >>> childEl
  renderFn this = getState this <#> applyProps

  getMappedProps props =
    getStoreState props.store <#>
      \appState -> build (storeSelector appState) props

  spec this = getProps this >>= getMappedProps <#> \mappedProps ->
    { state: { subscription: Nothing, mappedProps }
    , render: renderFn this

    , unsafeComponentWillMount: do
        let listener = \{ prevState, nextState } -> do
              let appState = fromMaybe prevState nextState
              x <- getProps this <#> build (storeSelector appState)
              modifyState this _ { mappedProps = x }

        { store } <- getProps this
        subscription   <- subscribe store listener
        modifyState this _ { subscription = Just subscription }

    , unsafeComponentWillReceiveProps: \nextProps -> do
        x <- getMappedProps nextProps
        modifyState this _ { mappedProps = x }

    , componentWillUnmount: do
        { store }   <- getProps this
        { subscription } <- getState this

        case subscription of
             Nothing -> pure unit
             Just x  -> unsubscribe x
    }
