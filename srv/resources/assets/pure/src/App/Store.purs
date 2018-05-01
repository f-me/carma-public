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
--   3. Subscribe to the store updates by `subscribe`
--      (e.g. inside `componentWillMount`), it returns unique
--      `StoreSubscription` which you could use to `unsubscribe`
--      (e.g. inside `componentWillUnmount`).
--      To `subscribe` you need a `StoreListener`,
--      to make one use `toStoreListener`
--      (it wraps some `Eff` monad that takes an update context as an argument).
--      Subscriber will be notified only if state is changed by a reducer
--      (use `subscribe'` to get notifications every time action is raised,
--      notwithstanding if state changes or not);
--
--   4. `dispatch` some actions any time you want, store reducer passed in 2st
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
     , StoreListener
     , StoreUpdateContext

     , createAppContext
     , reduceLoop

     , getAppState
     , dispatch
     , toStoreListener
     , subscribe
     , subscribe'
     , unsubscribe
     ) where

import Prelude
import Unsafe.Coerce (unsafeCoerce)

import Data.Map (Map, empty, insert, delete, filter)
import Data.Tuple (Tuple (Tuple), fst)
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Foldable (foldM)
import Data.JSDate (now, getTime)
import Data.List.Lazy (replicate)

import Control.Monad.Rec.Class (forever)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR, AVar , makeEmptyVar, takeVar, putVar)

import Control.Monad.Eff.Ref
     ( Ref, REF
     , newRef, readRef, writeRef, modifyRef, modifyRef'
     )

import App.Store.Actions (AppAction)
import App.Store.Reducers (AppState)


-- An identity of a subscrition that could be used to `unsubscribe`.
-- A `Ref` indicate is subscription alive or not (unsubscribed),
-- this fixes triggering after unsubscribing.
newtype StoreSubscription =
  StoreSubscription (Tuple SubscriberId (Ref Boolean))
instance eqStoreSubscription :: Eq StoreSubscription where
  eq (StoreSubscription (Tuple a _)) (StoreSubscription (Tuple b _)) = eq a b


-- An abstraction for a foreign subscriber which is an `Eff` monad
foreign import data StoreListener :: Type

-- This is an abstraction for `StoreListener` with `Boolean` mark
-- that indicates if a subscriber strict or not that means
-- will it be notified even if state wasn't changed.
-- A `Ref` indicate is subscription alive or not (unsubscribed),
-- this fixes triggering after unsubscribing.
type Subscriber = Tuple Boolean (Tuple StoreListener (Ref Boolean))

-- Converts foreign `Eff` monad to an abstract `StoreListener`
toStoreListener
  :: forall eff . (StoreUpdateContext -> Eff eff Unit) -> StoreListener
toStoreListener = unsafeCoerce

callStoreListener
  :: forall eff . StoreListener -> StoreUpdateContext -> Eff eff Unit
callStoreListener = unsafeCoerce


type StoreUpdateContext =
  { prevState :: AppState
  , nextState :: Maybe AppState
  , action    :: AppAction
  }


type SubscribersMap = Map SubscriberId Subscriber


newtype AppContext
  = AppContext
  { store       :: Ref AppState
  , subscribers :: Ref SubscribersMap
  , actionsBus  :: AVar AppAction

  -- Only one reducer loop is allowed!
  , isReduceLoopStarted :: Ref Boolean
  }


newtype SubscriberId = SubscriberId String
derive instance eqStoreSubscriberId  :: Eq SubscriberId
derive instance ordStoreSubscriberId :: Ord SubscriberId

newSubscriberId
  :: forall eff . Eff (now :: NOW, random :: RANDOM | eff) SubscriberId
newSubscriberId = do
  timeMark <- now <#> getTime >>> show

  randMark <- foldM (\acc x -> x <#> show >>> (acc <> _)) ""
            $ replicate 10
            $ randomInt 0 9

  pure $ SubscriberId $ timeMark <> "_" <> randMark


createAppContext
  :: forall eff
   . AppState
  -> Aff ( ref    :: REF
         , avar   :: AVAR
         , now    :: NOW
         , random :: RANDOM
         | eff
         ) AppContext

createAppContext initialState = do
  (store               :: Ref AppState)       <- liftEff $ newRef initialState
  (subscribers         :: Ref SubscribersMap) <- liftEff $ newRef empty
  (isReduceLoopStarted :: Ref Boolean)        <- liftEff $ newRef false
  (actionsBus          :: AVar AppAction)     <- makeEmptyVar

  pure $ AppContext
       { store
       , subscribers
       , actionsBus
       , isReduceLoopStarted
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

reduceLoop appCtx@(AppContext ctx) appReducer = reactToAction \action -> do

  subscriberData <-
    modifyRef' ctx.store $ \state ->
      let
        reduced = appReducer state action
      in
        { state: fromMaybe state reduced
        , value: { prevState: state, nextState: reduced, action }
        }

  subscribersMap <- readRef ctx.subscribers

  notify subscriberData $
    case subscriberData.nextState of
         -- State wasn't changed, notifying only strict subscribers
         Nothing -> filter fst subscribersMap
         -- State was changed, notifying all subscribers
         Just _  -> subscribersMap

  where
    notify updateCtx = foldM (\_ (Tuple _ x) -> f x) unit
      where
        f (Tuple x aliveRef) = do
          isAlive <- readRef aliveRef
          if isAlive then callStoreListener x updateCtx else pure unit

    guardOnlyOneInstance = do
      isReduceLoopStarted <- readRef ctx.isReduceLoopStarted

      if isReduceLoopStarted -- `when` here would fail because of strictness
         then unsafeThrow "Only one reduce loop is allowed"
         else pure unit

      writeRef ctx.isReduceLoopStarted true

    reactToAction m = do
      liftEff guardOnlyOneInstance
      forever $ takeVar ctx.actionsBus >>= liftEff <<< m


getAppState
  :: forall eff
   . AppContext
  -> Eff (ref :: REF | eff) AppState

getAppState (AppContext { store }) = readRef store


dispatch
  :: forall eff
   . AppContext
  -> AppAction
  -> Aff (avar :: AVAR | eff) Unit

dispatch (AppContext { actionsBus }) action = putVar action actionsBus


-- See `subscribeInternal` for details.
subscribe
  :: forall eff
   . AppContext
  -> StoreListener
  -> Eff ( ref    :: REF
         , now    :: NOW
         , random :: RANDOM
         | eff
         ) StoreSubscription

subscribe = subscribeInternal false

-- Strict version of `subscribe`, it means that subscriber will be notified
-- notwithstanding if state change or not (useful for side-effects handlers).
subscribe'
  :: forall eff
   . AppContext
  -> StoreListener
  -> Eff ( ref    :: REF
         , now    :: NOW
         , random :: RANDOM
         | eff
         ) StoreSubscription

subscribe' = subscribeInternal true


-- Attempt to `unsubscribe` multiple times for same subscriber
-- will case NO errors, we take it as okay.
unsubscribe
  :: forall eff
   . AppContext
  -> StoreSubscription
  -> Eff (ref :: REF | eff) Unit

unsubscribe (AppContext { subscribers })
            (StoreSubscription (Tuple subscriberId aliveRef)) = do

  writeRef aliveRef false
  modifyRef subscribers $ delete subscriberId


subscribeInternal
  :: forall eff
   . Boolean
  -> AppContext
  -> StoreListener
  -> Eff ( ref    :: REF
         , now    :: NOW
         , random :: RANDOM
         | eff
         ) StoreSubscription

subscribeInternal isStrict (AppContext { subscribers }) storeListener = do
  subscriberId <- newSubscriberId
  aliveRef     <- newRef true

  modifyRef' subscribers \subscribersMap ->
    { value: StoreSubscription $ Tuple subscriberId aliveRef
    , state: insert subscriberId
               (Tuple isStrict (Tuple storeListener aliveRef))
               subscribersMap
    }
