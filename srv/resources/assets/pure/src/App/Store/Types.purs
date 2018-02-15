module App.Store.Types
     ( StoreReducer
     , StoreSubscriber

     , StoreConnectEffects
     , AppContextEffects
     , StoreEffects
     ) where

import Prelude

import Data.Maybe (Maybe)

import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Aff (Aff)

import DOM (DOM)
import React (ReactProps, ReactState, ReactRefs, ReadWrite, ReadOnly)

import App.Store.Actions (AppAction)
import App.Store.Reducers (AppState)


-- `Maybe` here to be able to avoid notifying subscribers
-- (when state isn't changed for example).
type StoreReducer =
  AppState -> AppAction -> Maybe AppState

type StoreSubscriber eff =
  AppState -> AppAction -> Aff (ref :: REF | eff) Unit


type StoreConnectEffects eff =
  ( props :: ReactProps
  , state :: ReactState ReadWrite
  , refs  :: ReactRefs  ReadOnly
  , ref   :: REF
  | eff
  ) :: # Effect

type AppContextEffects eff =
  StoreConnectEffects ( console :: CONSOLE
                      , dom :: DOM
                      | eff
                      )

type StoreEffects eff =
  StoreConnectEffects ( console :: CONSOLE
                      , dom :: DOM
                      , ref :: REF
                      | eff
                      )
