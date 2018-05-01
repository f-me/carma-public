module Utils.StoreConnect
     ( StoreSelector
     , storeConnect
     ) where

import Prelude

import Data.Maybe (Maybe (..), fromMaybe)
import Data.Record.Builder (Builder, build)

import React
     ( ReactClass
     , createClass, spec', createElement
     , transformState, readState, getProps
     )

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

storeConnect storeSelector child = createClass spec
  where
    childEl = createElement child

    renderFn this = do
      state <- readState this
      pure $ childEl state.mappedProps []

    initialState this = do
      props    <- getProps this
      appState <- getAppState props.appContext

      pure { subscription : Nothing
           , mappedProps  : build (storeSelector appState) props
           }

    spec = spec' initialState renderFn # _
      { displayName = "StoreConnect"

      , componentWillMount = \this -> do
          let listener = toStoreListener \{ prevState, nextState } -> do
                let appState = fromMaybe prevState nextState
                x <- getProps this <#> build (storeSelector appState)
                transformState this $ _ { mappedProps = x }

          { appContext } <- getProps this
          subscription   <- subscribe appContext listener
          transformState this $ _ { subscription = Just subscription }

      , componentWillReceiveProps = \this nextProps -> do
          appState <- getAppState nextProps.appContext
          let x = build (storeSelector appState) nextProps
          transformState this $ _ { mappedProps = x }

      , componentWillUnmount = \this -> do
          { appContext } <- getProps this
          { subscription } <- readState this

          case subscription of
               Nothing -> pure unit
               Just x  -> unsubscribe appContext x
      }
