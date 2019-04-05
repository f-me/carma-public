-- This implementation mostly inspired by redux.js library, so you could find
-- parallels to understand it faster if you already met the redux.js library.
--
-- Some usage steps:
--
--   1. Create a store by `createStore` function passing reducer function to it
--      (which maps old state to new one looking at received action, it will be
--      called every time an action is triggered) and initial state value;
--
--   2. Subscribe to the store updates by `subscribe`
--      (e.g. inside `componentWillMount`), it returns unique
--      `StoreSubscription` which you could use to `unsubscribe`
--      (e.g. inside `componentWillUnmount`).
--      Subscriber will be notified only if state is changed by a reducer
--      (use `subscribe'` to get notifications every time action is raised,
--      notwithstanding if state is changed or not);
--
--   3. `dispatch` some actions any time you want, store reducer passed in 1st
--      step handles state updates looking at actions you dispatch, and
--      subscribers can trigger some side-effect such as API requests looking at
--      actions you dispatch (handling the same action at the same time in
--      reducer you could set `isLoading` flag in store for example) and they
--      could dispatch another actions with some response data (to save a result
--      in the store for example);
--
--   4. Do not forget to `unsubscribe` using `StoreSubscription` in
--      `componentWillUnmount`.
--
-- The purpuse of this is to create efficient state storage. You supposed to use
-- some HOC (High-Order Component) to bind to specific values from store and
-- pass them as properties to a component, so when state updates a component
-- will rerender but in this case parent component isn't urged to be rerendered,
-- `shouldComponentUpdate` (of parent component) could return `false` and child
-- component that attached to the store is still able to update any time. This
-- solves many performance issues (as opposite to using state from
-- root-component that requires rerendering whole tree).
-- P.S. By "rerendering" I meant even just constructing virtual-dom that still
-- wastes CPU time a lot.
--
-- This store implementation is attached to `AppState`, so it isn't polymorphic
-- for different state types, just for now, because we don't need more, but with
-- some modifications this store implementation could be separated and
-- generalized, so you could have different stores.

module App.Store
     ( AppContext
     , StoreSubscription
     , StoreListener
     , StoreUpdateContext
     , StoreReducer

     , createStore
     , getStoreState
     , dispatch
     , subscribe
     , subscribe'
     , unsubscribe
     ) where

import Prelude

import Data.Map (Map, empty, insert, delete, filter)
import Data.Tuple (Tuple (Tuple), fst, snd)
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Either (Either (..))
import Data.Foldable (class Foldable, foldM)

import Control.Monad.Rec.Class (forever)
import Control.Monad.Error.Class (throwError)

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, runAff_)
import Effect.Aff.AVar (put, take) as AffAVar
import Effect.AVar (AVar, empty) as AVar
import Effect.Ref as Ref
import Effect.Console (error)
import Effect.Exception (message)

import Web.HTML (window)
import Web.HTML.Window (alert)

import App.Store.Actions (AppAction)
import App.Store.Reducers (AppState)
import Utils.SubscriberId (SubscriberId, newSubscriberId)


-- An identity of a subscrition that could be used to `unsubscribe`.
-- A `Ref` indicate is subscription alive or not (unsubscribed),
-- this fixes triggering after unsubscribing.
data StoreSubscription =
     StoreSubscription SubscriberId (Ref.Ref Boolean) (Effect Unit)

instance eqStoreSubscription :: Eq StoreSubscription where
  eq (StoreSubscription a _ _) (StoreSubscription b _ _) = eq a b


type StoreListener = StoreUpdateContext -> Effect Unit

type StoreUpdateContext =
   { prevState :: AppState
   , nextState :: Maybe AppState
   , action    :: AppAction
   }

type StoreReducer
   = AppState
  -> AppAction
  -> Maybe AppState
  -- ^ `Maybe` here to be able to avoid notifying subscribers
  --   (when state isn't changed for example).

-- This is an abstraction for `StoreListener` with `Boolean` mark
-- that indicates if a subscriber strict or not that means
-- will it be notified even if state wasn't changed.
-- A `Ref` indicate is subscription alive or not (unsubscribed),
-- this fixes triggering after unsubscribing.
type Subscriber = Tuple Boolean (Tuple StoreListener (Ref.Ref Boolean))


type SubscribersMap = Map SubscriberId Subscriber


newtype AppContext
      = AppContext
      { store       :: Ref.Ref AppState
      , subscribers :: Ref.Ref SubscribersMap
      , actionsBus  :: AVar.AVar AppAction
      }


createStore :: StoreReducer -> AppState -> Effect AppContext
createStore storeReducer initState = go where
  go = do
    (store       :: Ref.Ref AppState)       <- Ref.new initState
    (subscribers :: Ref.Ref SubscribersMap) <- Ref.new empty
    (actionsBus  :: AVar.AVar AppAction)    <- AVar.empty

    -- Running store reducing thread.
    runAff_ reducerFailureHandler
      $ reduce actionsBus
      $ actionHandler store subscribers

    pure $ AppContext
         { store
         , subscribers
         , actionsBus
         }

  reducerFailureHandler (Right _) = pure unit
  reducerFailureHandler (Left err) = do
    error $ "Store reducer thread is failed with exception: " <> message err

    window >>= alert
      "Что-то пошло не так! Настоятельно рекомендуется перезагрузить\
      \ страницу для продолжения нормальной работы системы!"

    throwError err

  reduce actionsBus actionHandler' = forever reactToAction where
    reactToAction = AffAVar.take actionsBus >>= liftEffect <<< actionHandler'

  actionHandler store subscribers action = do
    subscriberData <-
      let f state = go' where
            reduced = storeReducer state action
            go' = { state: fromMaybe state reduced
                  , value: { prevState: state, nextState: reduced, action }
                  }

       in f `Ref.modify'` store

    subscribersMap <- Ref.read subscribers

    notify subscriberData $
      case subscriberData.nextState of
           -- State wasn't changed, notifying only strict subscribers
           Nothing -> filter fst subscribersMap
           -- State was changed, notifying all subscribers
           Just _  -> subscribersMap

  notify
    :: forall f. Foldable f => StoreUpdateContext -> f Subscriber -> Effect Unit

  notify updateCtx = foldM (const $ snd >>> notifyListener) unit where
    notifyListener (Tuple storeListener aliveRef) = do
      isAlive <- Ref.read aliveRef
      if isAlive then storeListener updateCtx else pure unit


getStoreState :: AppContext -> Effect AppState
getStoreState (AppContext { store }) = Ref.read store


dispatch :: AppContext -> AppAction -> Aff Unit
dispatch (AppContext { actionsBus }) action = AffAVar.put action actionsBus


-- See `subscribeInternal` for details.
subscribe :: AppContext -> StoreListener -> Effect StoreSubscription
subscribe = subscribeInternal false

-- Strict version of `subscribe`, it means that subscriber will be notified
-- notwithstanding if state change or not (useful for side-effects handlers).
subscribe' :: AppContext -> StoreListener -> Effect StoreSubscription
subscribe' = subscribeInternal true


-- Attempt to `unsubscribe` multiple times for same subscriber
-- will case NO errors, we take it as okay.
unsubscribe :: StoreSubscription -> Effect Unit
unsubscribe (StoreSubscription _ _ unsubscriber) = unsubscriber


subscribeInternal
  :: Boolean
  -> AppContext
  -> StoreListener
  -> Effect StoreSubscription

subscribeInternal isStrict (AppContext { subscribers }) storeListener = do
  subscriberId <- newSubscriberId
  aliveRef     <- Ref.new true

  let unsubscriber = do
        false `Ref.write` aliveRef
        delete subscriberId `Ref.modify_` subscribers

  let f subscribersMap =
        { value: StoreSubscription subscriberId aliveRef unsubscriber
        , state: insert subscriberId
                   (Tuple isStrict (Tuple storeListener aliveRef))
                   subscribersMap
        }

  f `Ref.modify'` subscribers
