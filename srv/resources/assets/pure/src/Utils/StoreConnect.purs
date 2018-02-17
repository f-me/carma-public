module Utils.StoreConnect
     ( StoreSelector
     , storeConnect
     ) where

import Prelude

import Data.Maybe (Maybe (..))
import Data.Record.Builder (Builder, build)

import Control.Monad.Rec.Class (forever)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Aff (launchAff_)
import Control.Monad.Aff.AVar (takeVar)

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

          launchAff_ $ do
            bus <- getSubscriberBus props.appContext subscription

            let transformer appState = do
                  x <- getProps this <#> build (storeSelector appState)
                  transformState this $ _ { mappedProps = x }

                catchUnsubscribed err = do
                  if message err == "Unsubscribed"
                     then pure unit -- It's okay, we're done
                     else throwError err -- Unknown exception

            flip catchError catchUnsubscribed $ forever $ do
              event <- takeVar bus
              liftEff $ transformer event.state

          transformState this $ _ { subscription = Just subscription }

      , componentWillUnmount = \this -> do
          props <- getProps  this
          state <- readState this

          case state.subscription of
               Nothing -> pure unit
               Just x  -> unsubscribe props.appContext x
      }
