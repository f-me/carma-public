-- This store implementation is attached to `AppState`, so it isn't polymorphic
-- for different state types, just for now, because we don't need more, but with
-- some modifications this store implementation could be separated and
-- generalized, so you could have different stores.

module App.Store
     ( AppContext
     , AppState
     , AppAction (..)
     , StoreSubscription
     , createAppContext
     , getAppState
     , dispatch
     , subscribe
     , unsubscribe
     ) where

import Prelude

import Data.Map (Map, empty, insert, delete)
import Data.Tuple (Tuple (Tuple), snd)
import Data.Maybe (Maybe (..))
import Data.Foldable (foldM)

import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref ( Ref, REF
                             , newRef, readRef, writeRef, modifyRef, modifyRef'
                             )

import Router (Location)


type AppState =
  { currentLocation :: Location
  }

data AppAction
  = Navigate Location

newtype StoreSubscription = StoreSubscription Int
derive instance eqStoreSubscriberId :: Eq StoreSubscription
derive instance ordStoreSubscriberId :: Ord StoreSubscription

-- `Maybe` here to be able to avoid notifying subscribers
-- (when state isn't changed for example).
type StoreReducer =
  AppState -> AppAction -> Maybe AppState

type StoreSubscriber eff =
  AppState -> AppAction -> Aff (ref :: REF | eff) Unit

type StoreSubscribers eff =
  Tuple Int
        -- ^ Unique id value
        (Map StoreSubscription (StoreSubscriber eff))
        -- ^ Map of subscribers keyed by unique id

data AppContext subscriberEff
  = AppContext
  { store       :: Ref AppState
  , reducer     :: StoreReducer
  , subscribers :: Ref (StoreSubscribers subscriberEff)
  }


createAppContext
  :: forall eff
   . StoreReducer
  -> AppState
  -> Eff (ref :: REF | eff) (AppContext eff)

createAppContext storeReducer initialState = do
  (storeRef       :: Ref AppState)               <- newRef initialState
  (subscribersRef :: Ref (StoreSubscribers eff)) <- newRef $ Tuple 1 empty

  pure $ AppContext
       { store: storeRef
       , reducer: storeReducer
       , subscribers: subscribersRef
       }


getAppState :: forall eff. AppContext eff -> Eff (ref :: REF | eff) AppState
getAppState (AppContext ctx) = readRef ctx.store


dispatch
  :: forall eff
   . AppContext eff
  -> AppAction
  -> Eff (ref :: REF | eff) Unit

dispatch (AppContext ctx) action = do
  appState         <- readRef ctx.store
  storeSubscribers <- readRef ctx.subscribers <#> snd

  newAppState <-
    case ctx.reducer appState action of
         Just x  -> x <$ writeRef ctx.store x
         Nothing -> pure appState

  let f acc x = acc <$ launchAff_ (x newAppState action)
  foldM f unit storeSubscribers


subscribe
  :: forall eff
   . AppContext eff
  -> StoreSubscriber eff
  -> Eff (ref :: REF | eff) StoreSubscription

subscribe (AppContext ctx) subscriber =
  modifyRef' ctx.subscribers $ \(Tuple nextId s) ->
    let
      subscription = StoreSubscription nextId
      updated = Tuple (nextId + 1) $ insert subscription subscriber s
    in
      {state: updated, value: subscription}


unsubscribe
  :: forall eff
   . AppContext eff
  -> StoreSubscription
  -> Eff (ref :: REF | eff) Unit

unsubscribe (AppContext ctx) subscription =
  modifyRef ctx.subscribers $ \(Tuple nextId s) ->
    Tuple nextId $ delete subscription s
