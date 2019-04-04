-- This implementation mostly inspired by redux.js library, so you could find
-- parallels to understand it faster if you already met the redux.js library.
--
-- Some usage steps:
--
--   1. Create a store by `createAppContext` passing initial state to it;
--
--   2. Run `Aff` thread with started `reduceLoop` to update state by actions
--      and notify subscribers when state changes (n.b. strict subscribers will
--      be notified even if state haven't changed), only one thread with started
--      `reduceLoop` is allowed;
--
--   3. Subscribe to the store updates by `subscribe`
--      (e.g. inside `componentWillMount`), it returns unique
--      `StoreSubscription` which you could use to `unsubscribe`
--      (e.g. inside `componentWillUnmount`).
--      Subscriber will be notified only if state is changed by a reducer
--      (use `subscribe'` to get notifications every time action is raised,
--      notwithstanding if state is changed or not);
--
--   4. `dispatch` some actions any time you want, store reducer passed in 2nd
--      step handles state updates looking at actions you dispatch, and
--      subscribers can trigger some side-effect such as API requests looking at
--      actions you dispatch and they could dispatch another actions with some
--      response data;
--
--   5. Do not forget to `unsubscribe` using `StoreSubscription` in
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

     , createAppContext
     , reduceLoop

     , getAppState
     , dispatch
     , subscribe
     , subscribe'
     , unsubscribe
     ) where

import Prelude

import Data.Map (Map, empty, insert, delete, filter)
import Data.Tuple (Tuple (Tuple), fst)
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Foldable (foldM)

import Control.Monad.Rec.Class (forever)

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Aff (Aff)
import Effect.Aff.AVar as AVar
import Effect.Ref as Ref

import App.Store.Actions (AppAction)
import App.Store.Reducers (AppState)
import Utils.SubscriberId (SubscriberId, newSubscriberId)


-- An identity of a subscrition that could be used to `unsubscribe`.
-- A `Ref` indicate is subscription alive or not (unsubscribed),
-- this fixes triggering after unsubscribing.
newtype StoreSubscription =
  StoreSubscription (Tuple SubscriberId (Ref.Ref Boolean))
instance eqStoreSubscription :: Eq StoreSubscription where
  eq (StoreSubscription (Tuple a _)) (StoreSubscription (Tuple b _)) = eq a b

type StoreListener = StoreUpdateContext -> Effect Unit

-- This is an abstraction for `StoreListener` with `Boolean` mark
-- that indicates if a subscriber strict or not that means
-- will it be notified even if state wasn't changed.
-- A `Ref` indicate is subscription alive or not (unsubscribed),
-- this fixes triggering after unsubscribing.
type Subscriber = Tuple Boolean (Tuple StoreListener (Ref.Ref Boolean))


type StoreUpdateContext =
   { prevState :: AppState
   , nextState :: Maybe AppState
   , action    :: AppAction
   }


type SubscribersMap = Map SubscriberId Subscriber


newtype AppContext
      = AppContext
      { store       :: Ref.Ref AppState
      , subscribers :: Ref.Ref SubscribersMap
      , actionsBus  :: AVar.AVar AppAction

      -- Only one reducer loop is allowed!
      , isReduceLoopStarted :: Ref.Ref Boolean
      }


createAppContext :: AppState -> Aff AppContext
createAppContext initState = do
  (store :: Ref.Ref AppState) <- liftEffect $ Ref.new initState
  (subscribers :: Ref.Ref SubscribersMap) <- liftEffect $ Ref.new empty
  (isReduceLoopStarted :: Ref.Ref Boolean) <- liftEffect $ Ref.new false
  (actionsBus :: AVar.AVar AppAction) <- AVar.empty

  pure $ AppContext
       { store
       , subscribers
       , actionsBus
       , isReduceLoopStarted
       }


reduceLoop
  :: AppContext
  -> (
       AppState
       -> AppAction
       -> Maybe AppState
       -- ^ `Maybe` here to be able to avoid notifying subscribers
       --   (when state isn't changed for example).
     )
  -> Aff Unit

reduceLoop appCtx@(AppContext ctx) appReducer = go where
  go = reactToAction actionHandler

  actionHandler action = do
    subscriberData <-
      let f state = go' where
            reduced = appReducer state action
            go' = { state: fromMaybe state reduced
                  , value: { prevState: state, nextState: reduced, action }
                  }

       in f `Ref.modify'` ctx.store

    subscribersMap <- Ref.read ctx.subscribers

    notify subscriberData $
      case subscriberData.nextState of
           -- State wasn't changed, notifying only strict subscribers
           Nothing -> filter fst subscribersMap
           -- State was changed, notifying all subscribers
           Just _  -> subscribersMap

  notify updateCtx = foldM (\_ (Tuple _ x) -> f x) unit
    where
      f (Tuple x aliveRef) = do
        isAlive <- Ref.read aliveRef
        if isAlive then x updateCtx else pure unit

  guardOnlyOneInstance = do
    isReduceLoopStarted <- Ref.read ctx.isReduceLoopStarted

    if isReduceLoopStarted -- `when` here would fail because of strictness
       then unsafeThrow "Only one reduce loop is allowed"
       else pure unit

    true `Ref.write` ctx.isReduceLoopStarted

  reactToAction m = do
    liftEffect guardOnlyOneInstance
    forever $ AVar.take ctx.actionsBus >>= liftEffect <<< m


getAppState :: AppContext -> Effect AppState
getAppState (AppContext { store }) = Ref.read store


dispatch :: AppContext -> AppAction -> Aff Unit
dispatch (AppContext { actionsBus }) action = AVar.put action actionsBus


-- See `subscribeInternal` for details.
subscribe :: AppContext -> StoreListener -> Effect StoreSubscription
subscribe = subscribeInternal false

-- Strict version of `subscribe`, it means that subscriber will be notified
-- notwithstanding if state change or not (useful for side-effects handlers).
subscribe' :: AppContext -> StoreListener -> Effect StoreSubscription
subscribe' = subscribeInternal true


-- Attempt to `unsubscribe` multiple times for same subscriber
-- will case NO errors, we take it as okay.
unsubscribe :: AppContext -> StoreSubscription -> Effect Unit
unsubscribe (AppContext { subscribers })
            (StoreSubscription (Tuple subscriberId aliveRef)) = do

  false `Ref.write` aliveRef
  delete subscriberId `Ref.modify_` subscribers


subscribeInternal
  :: Boolean
  -> AppContext
  -> StoreListener
  -> Effect StoreSubscription

subscribeInternal isStrict (AppContext { subscribers }) storeListener = do
  subscriberId <- newSubscriberId
  aliveRef     <- Ref.new true

  let f subscribersMap =
        { value: StoreSubscription $ Tuple subscriberId aliveRef
        , state: insert subscriberId
                   (Tuple isStrict (Tuple storeListener aliveRef))
                   subscribersMap
        }

  f `Ref.modify'` subscribers
