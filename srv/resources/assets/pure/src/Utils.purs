module Utils
     ( StoreConnectEff
     , storeConnect
     ) where

import Prelude

import Data.Maybe (Maybe (..))
import Data.Record.Builder (Builder, build)

import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Aff (liftEff')

import React ( ReactSpec, ReactThis, ReactClass
             , ReactProps, ReactState, ReactRefs
             , ReadWrite, ReadOnly
             , createClass, spec', createElement
             , transformState, readState
             , getProps
             )

import App.Store ( StoreSubscriber, StoreSubscription
                 , AppContext, AppAction, AppState
                 , subscribe, unsubscribe, getAppState
                 )


type StoreConnectEff eff =
  ( props :: ReactProps
  , state :: ReactState ReadWrite
  , refs  :: ReactRefs  ReadOnly
  , ref   :: REF
  | eff
  ) :: # Effect

storeConnect
  :: forall eff props1 props2
   . AppContext
   ( props :: ReactProps
   , state :: ReactState ReadWrite
   , refs  :: ReactRefs  ReadOnly
   , ref   :: REF
   | eff
   )
  -> (AppState -> Builder (Record props1) (Record props2))
  -> ReactClass (Record props2)
  -> ReactClass (Record props1)

storeConnect store storeSelector child = createClass spec

  where

    renderFn this = do
      state <- readState this
      pure $ createElement child state.mappedProps []

    initialState this = do
      -- TODO FIXME `unsafeCoerceEff` to avoid:
      --            couldn't match `ReadOnly` with type `Disallowed`
      --            (don't know why yet)
      appState <- unsafeCoerceEff $ getAppState store
      props <- getProps this

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

          subscription <- subscribe store $ storeUpdateHandler transformer
          transformState this $ _ { subscription = Just subscription }

      , componentWillUnmount = \this -> do
          state <- readState this

          -- TODO FIXME `unsafeCoerceEff` to avoid:
          --            couldn't match `ReadOnly` with type `ReadWrite`
          --            inside `ReactState`
          --            (don't know why yet)
          case state.subscription of
               Just x  -> unsafeCoerceEff $ unsubscribe store x
               Nothing -> pure unit
      }
