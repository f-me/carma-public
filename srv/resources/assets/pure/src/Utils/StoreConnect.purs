module Utils.StoreConnect
     ( StoreSelector
     , storeConnect
     ) where

import Prelude

import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (Tuple))
import Data.Record.Builder (Builder, build)

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Aff (launchAff, launchAff_, killFiber)
import Control.Monad.Aff.AVar (takeVar)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)

import React
     ( ReactClass
     , createClass, spec', createElement
     , transformState, readState, getProps
     )

import App.Store.Reducers (AppState)

import App.Store
     ( AppContext
     , subscribe, unsubscribe, getAppState, getSubscriberBus
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

    renderFn this = do
      state <- readState this
      pure $ createElement child state.mappedProps []

    initialState this = do
      props    <- getProps this
      appState <- getAppState props.appContext

      pure { subscription : Nothing
           , mappedProps  : build (storeSelector appState) props
           }

    spec = spec' initialState renderFn # _
      { displayName = "StoreConnect"

      , componentWillMount = \this -> do
          props        <- getProps  this
          subscription <- subscribe props.appContext

          fiber <- launchAff $ do
            bus <- getSubscriberBus props.appContext subscription

            let transformer appState = do
                  x <- getProps this <#> build (storeSelector appState)
                  transformState this $ _ { mappedProps = x }

                recursiveReact = do
                  event <- takeVar bus
                  liftEff $ transformer event.state
                  recursiveReact

            recursiveReact

          transformState this $ _
            { subscription = Just (Tuple subscription fiber) }

      , componentWillUnmount = \this -> do
          props <- getProps  this
          state <- readState this

          case state.subscription of
               Nothing -> pure unit
               Just (Tuple subscription fiber) -> launchAff_ $ do

                 -- `unsafeCoerceAff` because fiber inherited effects from
                 -- `componentWillMount`, there's some differences in
                 -- `ReactState` and `ReactRefs`, I have no idea yet how to
                 -- solve this.
                 unsafeCoerceAff $
                   killFiber (error "Component was unmounted") fiber

                 liftEff $ unsubscribe props.appContext subscription
      }
