-- This implementation mostly inspired by redux.js library, so you could find
-- parallels to understand it faster if you already met the redux.
--
-- Some usage steps:
--
--   1. Create a store by `createAppContext` passing initial state to it;
--
--   2. Run `Aff` thread with started `reduceLoop` to update state by actions
--      and notify subscribers when state change (n.b. strict subscribers will
--      be notified even if state haven't changed), only one thread with started
--      `reduceLoop` is allowed;
--
--   3. Subscribe to store updates by `subscribe`
--      (e.g. inside `componentWillMount`), it returns unique
--      `StoreSubscription` which you could use to unsubscribe (e.g. in
--      `componentWillUnmount`) and to get a bus to read events from,
--      subscriber will be notified only if state change by reducer (use
--      `subscribe'` to get notifications every time action is rised,
--      notwithstanding if state change or not);
--
--   4. Start another `Aff` thread and get unique `SubscriberBus` (`AVar`) by
--      passed `StoreSubscription` (a bus is unique for `StoreSubscription`),
--      then you could infinitely recursively read from bus in blocking mode by
--      `takeVar` and react on these events (for the same `StoreSubscription`
--      the same `AVar` bus will be returned), you could use a `Fiber` returned
--      by `forkAff` to kill it before unsubscribing;
--
--   5. `dispatch` some actions any time you want, store reducer passed in 2st
--      step handles state updates looking at actions you dispatch, and
--      subscribers can trigger some side-effect such as API requests looking at
--      actions you dispatch and they could dispatch another actions with some
--      response data;
--
--   6. Do not forget to `unsubscribe` using `StoreSubscription` in
--      `componentWillUnmount` (if you try to read from bus after you
--      unsubscribe it will fail).
--
-- The purpuse of this is to create efficient state storage. You supposed to use
-- some HOC (High-Order Component) to bind to specific values from store and
-- pass them as properties to a component, so when state is updated component
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
     , SubscriberBus

     , createAppContext
     , reduceLoop

     , getAppState
     , dispatch
     , subscribe
     , getSubscriberBus
     , subscribe'
     , unsubscribe
     ) where

import Prelude

import Data.Map (Map, empty, insert, delete, lookup, filter, update)
import Data.Tuple (Tuple (Tuple), fst, snd)
import Data.Maybe (Maybe (..), maybe, fromMaybe)
import Data.Either (Either (..))
import Data.Foldable (foldM)

import Control.Monad.Rec.Class (forever)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Control.Monad.Aff (Aff, launchAff_)

import Control.Monad.Eff.Ref
     ( Ref, REF
     , newRef, readRef, writeRef, modifyRef, modifyRef'
     )

import Control.Monad.Aff.AVar
     ( AVAR, AVar
     , makeEmptyVar, takeVar, putVar, killVar, isKilledVar
     )

import App.Store.Actions (AppAction)
import App.Store.Reducers (AppState)


newtype StoreSubscription = StoreSubscription Int
derive instance eqStoreSubscriberId  :: Eq StoreSubscription
derive instance ordStoreSubscriberId :: Ord StoreSubscription


type SubscriberBus =
  AVar { state :: AppState, action :: AppAction }


type Subscriber =

  Tuple

    Boolean
    -- ^ Is this subscriber strict or not
    --   (will it be notified even state wasn't changed)

    (Maybe SubscriberBus)


type Subscribers =

  Tuple

    Int
    -- ^ Unique id value

    (Map StoreSubscription Subscriber)
    -- ^ Map of subscribers keyed by unique id


newtype AppContext
  = AppContext
  { store       :: Ref AppState
  , subscribers :: Ref Subscribers
  , actionsBus  :: AVar AppAction

  -- Only one reducer loop is allowed
  , isReduceLoopStarted :: Ref Boolean
  }


createAppContext
  :: forall eff
   . AppState
  -> Aff (ref :: REF, avar :: AVAR | eff) AppContext

createAppContext initialState = do
  (store               :: Ref AppState)    <- liftEff $ newRef initialState
  (subscribers         :: Ref Subscribers) <- liftEff $ newRef $ Tuple 1 empty
  (isReduceLoopStarted :: Ref Boolean)     <- liftEff $ newRef false
  (actionsBus          :: AVar AppAction)  <- makeEmptyVar

  pure $ AppContext
       { store               : store
       , subscribers         : subscribers
       , actionsBus          : actionsBus
       , isReduceLoopStarted : isReduceLoopStarted
       }


reduceLoop
  :: forall eff
   . AppContext
  -> (
       AppState
       -> AppAction
       -> Maybe AppState
       -- ^ `Maybe` here to be able to avoid notifying subscribers
       --   (when state isn't changed for example).
     )
  -> Aff (ref :: REF, avar :: AVAR | eff) Unit

reduceLoop appCtx@(AppContext ctx) appReducer = guardOnlyOneInstance $ do
  action <- takeVar ctx.actionsBus

  newState <-
    liftEff $ modifyRef' ctx.store $ \state ->
      let
        reduced = appReducer state action
      in
        { state: fromMaybe state reduced
        , value: maybe (Left state) Right reduced
        }

  subscribers <- liftEff $ readRef ctx.subscribers <#> snd

  case newState of

       -- `Left` means state wasn't changed,
       -- so, we're notifying only strict subscribers.
       Left s  -> notify { state: s, action: action } $ filter fst subscribers

       -- `Right` means state was changed, we're notifying all subscribers.
       Right s -> notify { state: s, action: action } subscribers

  where
    notify x =

      let f acc (Tuple _ Nothing) = pure acc
          f acc (Tuple _ (Just bus)) = do
            isKilled <- isKilledVar bus
            if not isKilled
               then acc <$ putVar x bus
               else pure unit

       in foldM f unit

    guardOnlyOneInstance iter = do
      isReduceLoopStarted <- liftEff $ readRef ctx.isReduceLoopStarted

      if isReduceLoopStarted -- `when` will fail because of strictness
         then unsafeThrow "Only one reduce loop is allowed"
         else pure unit

      liftEff $ writeRef ctx.isReduceLoopStarted true
      forever iter


getAppState
  :: forall eff
   . AppContext
  -> Eff (ref :: REF | eff) AppState

getAppState (AppContext ctx) = readRef ctx.store


dispatch
  :: forall eff
   . AppContext
  -> AppAction
  -> Aff (avar :: AVAR | eff) Unit

dispatch (AppContext ctx) action = putVar action ctx.actionsBus


-- See `subscribeInternal` for details.
subscribe
  :: forall eff
   . AppContext
  -> Eff (ref :: REF | eff) StoreSubscription

subscribe = subscribeInternal false

-- Strict version of `subscribe`, it means that subscriber will be notified
-- notwithstanding if state change or not (useful for side-effects handlers).
subscribe'
  :: forall eff
   . AppContext
  -> Eff (ref :: REF | eff) StoreSubscription

subscribe' = subscribeInternal true


getSubscriberBus
  :: forall eff
   . AppContext
  -> StoreSubscription
  -> Aff (ref :: REF, avar :: AVAR | eff) SubscriberBus

getSubscriberBus (AppContext ctx) subscription = do
  subscribers <- liftEff $ readRef ctx.subscribers <#> snd

  let subscriber =
        case subscription `lookup` subscribers of
             Just x  -> x
             Nothing -> unsafeThrow $ "Subscriber not found: "
                          <> show (subscription # \(StoreSubscription x) -> x)

  case snd subscriber of
       Just x  -> pure x -- If `AVar` is already created just returning it
       Nothing -> do
         -- Creating new `AVar` and storing it in subscribers list
         (subscriberBus :: SubscriberBus) <- makeEmptyVar

         let subscriberBusF Nothing  = Just subscriberBus
             subscriberBusF (Just _) =
               unsafeThrow "Subscriber unexpectedly already have a bus"

             f = update (map subscriberBusF >>> Just) subscription

         liftEff $ modifyRef ctx.subscribers $ map f
         pure subscriberBus


-- After this you mustn't touch `AVar` any more, it will fail if you do so.
unsubscribe
  :: forall eff
   . AppContext
  -> StoreSubscription
  -> Eff (ref :: REF, avar :: AVAR | eff) Unit

unsubscribe (AppContext ctx) subscription = do
  subscriber <-
    modifyRef' ctx.subscribers $ \(Tuple nextId s) ->
      { state: Tuple nextId $ delete subscription s
      , value: lookup subscription s
      }

  case join $ map snd subscriber of
       Nothing -> pure unit
       Just x  -> launchAff_ $ killVar (error "Unsubscribed") x


-- It must be an `Eff` to return `StoreSubscription` synchronously, so
-- `componentWillMount` could immidiately store `StoreSubscription` to the
-- component's state. We can't create new `AVar` here because it needs `Aff`.
subscribeInternal
  :: forall eff
   . Boolean
  -> AppContext
  -> Eff (ref :: REF | eff) StoreSubscription

subscribeInternal isStrict (AppContext ctx) =
  modifyRef' ctx.subscribers $ \(Tuple nextId s) ->
    let
      subscription = StoreSubscription nextId

      updated = Tuple (nextId + 1)
              $ insert subscription (Tuple isStrict Nothing) s
    in
      { state: updated, value: subscription }
