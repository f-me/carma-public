-- This module implements typical "debounce" feature logic. If you're not
-- familiar with "debouncing" stuff, you could search over the internet for
-- explanation, there's plenty (there's also "throttle" which is little
-- different).
--
-- This could be used to fix performance issues for complex operations such as
-- triggering filtering of big data objects by changing value of an <input>
-- (e.g. debounce `onChange` to trigger filtering after a user is done typing).
--
-- Simple explanation of this feature (may be not the best one, you could find
-- in the internet better explanation even with graphical and animated
-- illustrations):
--   1. This is a middleware between actual handler and triggerer of that
--      handler (e.g. in context of <input> it is a middleware between
--      `onChange` handler and real end-point handler of such event);
--   2. A handler will be called only after specified gap of time
--      since middleware triggerer is called;
--   3. If you call middleware triggerer again before it handle previous call
--      (which means you do it before that gap of time is up)
--      it will forget previous call and will wait again for that gap of time.
--
module Utils.Debouncer
     ( Debouncer
     , DebouncerSubscription
     , DebouceTimeInMS
     , newDebouncer
     , sendToDebouncer
     , subscribeToDebouncer
     , unsubscribeFromDebouncer
     ) where

import Prelude

import Data.Maybe (Maybe (..))
import Data.Foldable (traverse_)
import Data.Map (Map, empty, insert, delete)

import Control.Monad.Rec.Class (forever)

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Timer (TimeoutId, setTimeout, clearTimeout)
import Effect.Ref as Ref
import Effect.Aff (launchAff_)
import Effect.Aff.AVar as AVar

import Utils.SubscriberId (SubscriberId, newSubscriberId)


type DebouceTimeInMS = Int

newtype DebouncerSubscription = DebouncerSubscription SubscriberId
derive instance eqDebouncerSubscription  :: Eq DebouncerSubscription
derive instance ordDebouncerSubscription :: Ord DebouncerSubscription

data Debouncer a
   = Debouncer
   { delay       :: DebouceTimeInMS
   , timerId     :: Ref.Ref (Maybe TimeoutId)
   , subscribers :: Ref.Ref (Map DebouncerSubscription (AVar.AVar a))
   }


-- Creating new `Debouncer` reference with specified delay time in milliseconds.
-- A `Debouncer` will be ate by garbage collector, so you only need to
-- unsubscribe from it your listeners.
newDebouncer :: forall a. DebouceTimeInMS -> Effect (Debouncer a)
newDebouncer delay = do
  timerId     <- Ref.new Nothing
  subscribers <- Ref.new empty
  pure $ Debouncer { delay, timerId, subscribers }


-- Send new value to `Debouncer`, this will debounce subscribers notifying with
-- specified delay (specified while creating new `Debouncer` by `newDebouncer`).
-- This will remove previous delayed trigger if it exists and will start again
-- waiting for whole delay time. After that time it will notify all subscribers.
sendToDebouncer :: forall a. Debouncer a -> a -> Effect Unit
sendToDebouncer (Debouncer { delay, timerId, subscribers }) newValue = do
  Ref.read timerId
    >>= case _ of
             Nothing -> pure unit
             Just x  -> clearTimeout x

  let delayedHandler = do
        Nothing `Ref.write` timerId
        Ref.read subscribers >>= traverse_ (launchAff_ <<< AVar.put newValue)

  setTimeout delay delayedHandler >>= Just >>> flip Ref.write timerId


-- Adding a listener to `Debouncer` which will be called when a debouncer will
-- get a value and some gap of time is up after that (debounced).
--
-- TODO use sync `Effect` `AVar` to replace subscription by some identity with
--      wrapped `AVar`.
subscribeToDebouncer
  :: forall a. Debouncer a -> (a -> Effect Unit) -> Effect DebouncerSubscription

subscribeToDebouncer (Debouncer { subscribers }) listener = do
  subscription <- newSubscriberId <#> DebouncerSubscription

  launchAff_ $ do
    var <- AVar.empty
    liftEffect $ insert subscription var `Ref.modify_` subscribers
    forever $ AVar.take var >>= liftEffect <<< listener

  pure subscription


-- Unsubscribe a listener from `Debouncer` so it will no longer be called by new
-- value sent to a debouncer.
unsubscribeFromDebouncer
  :: forall a. Debouncer a -> DebouncerSubscription -> Effect Unit

unsubscribeFromDebouncer (Debouncer { subscribers }) subscription =
  delete subscription `Ref.modify_` subscribers
