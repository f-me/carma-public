module Utils.StoreConnect
     ( StoreConnectEff
     , StoreConnectProps
     , StoreSelector
     , storeConnect
     ) where

import Prelude

import Data.Maybe (Maybe (..))
import Data.Record.Builder (Builder, build)

import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Aff (liftEff')

import React ( ReactClass, ReactProps, ReactState, ReactRefs
             , ReadWrite, ReadOnly
             , createClass, spec', createElement
             , transformState, readState
             , getProps
             )

import App.Store.Reducers (AppState)

import App.Store ( AppContext, StoreSubscription
                 , subscribe, unsubscribe, getAppState
                 )


type StoreConnectEff eff =
  ( props :: ReactProps
  , state :: ReactState ReadWrite
  , refs  :: ReactRefs  ReadOnly
  , ref   :: REF
  | eff
  ) :: # Effect

type StoreConnectProps eff props =
  { appContext :: AppContext ( props :: ReactProps
                             , state :: ReactState ReadWrite
                             , refs  :: ReactRefs  ReadOnly
                             , ref   :: REF
                             | eff
                             )
  | props
  }

type StoreSelector eff props1 props2
   = AppState
  -> Builder (StoreConnectProps eff props1) (StoreConnectProps eff props2)

storeConnect
  :: forall eff props1 props2
   . StoreSelector eff props1 props2
  -> ReactClass (StoreConnectProps eff props2)
  -> ReactClass (StoreConnectProps eff props1)

storeConnect storeSelector child = createClass spec

  where

    renderFn this = do
      state <- readState this
      pure $ createElement child state.mappedProps []

    initialState this = do
      props <- getProps this

      -- TODO FIXME `unsafeCoerceEff` to avoid:
      --            couldn't match `ReadOnly` with type `Disallowed`
      --            (don't know why yet)
      appState <- unsafeCoerceEff $ getAppState props.appContext

      pure { subscription : (Nothing :: Maybe StoreSubscription)
           , mappedProps  : build (storeSelector appState) props
           }

    storeUpdateHandler transformer appState _ =
      liftEff' $ transformer appState

    spec = spec' initialState renderFn # _
      { displayName = "StoreConnect"

      , componentDidMount = \this -> do
          let transformer appState = do
                props <- getProps this <#> build (storeSelector appState)
                transformState this $ _ { mappedProps = props }

          props <- getProps this

          subscription <-
            subscribe props.appContext $ storeUpdateHandler transformer

          transformState this $ _ { subscription = Just subscription }

      , componentWillUnmount = \this -> do
          props <- getProps this
          state <- readState this

          -- TODO FIXME `unsafeCoerceEff` to avoid:
          --            couldn't match `ReadOnly` with type `ReadWrite`
          --            inside `ReactState`
          --            (don't know why yet)
          case state.subscription of
               Just x  -> unsafeCoerceEff $ unsubscribe props.appContext x
               Nothing -> pure unit
      }
