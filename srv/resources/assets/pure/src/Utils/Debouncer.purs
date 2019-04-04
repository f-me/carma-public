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
import Control.Monad.Error.Class (catchError, throwError)

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Timer (TimeoutId, setTimeout, clearTimeout)
import Effect.Ref (Ref, new, read, write, modify_) as Ref
import Effect.AVar (AVar, empty, kill) as AVar
import Effect.Aff.AVar (put, take) as AffAVar
import Effect.Aff (launchAff_)
import Effect.Exception (error, message) as Exception

import Utils.SubscriberId (SubscriberId, newSubscriberId)


type DebouceTimeInMS = Int
data DebouncerSubscription = DebouncerSubscription SubscriberId (Effect Unit)

data Debouncer a
   = Debouncer
   { delay       :: DebouceTimeInMS
   , timerId     :: Ref.Ref (Maybe TimeoutId)
   , subscribers :: Ref.Ref (Map SubscriberId (AVar.AVar a))
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
  -- Clear previous timeout if has one.
  Ref.read timerId
    >>= case _ of
             Nothing -> pure unit
             Just x  -> clearTimeout x

  let -- Notifying subscribers about received value.
      delayedHandler = do
        Nothing `Ref.write` timerId
        Ref.read subscribers >>= traverse_ (launchAff_ <<< AffAVar.put newValue)

  -- Delaying notification and storing delayed timer id in `Ref`
  -- so when we receive a new value before previous timer is done
  -- we could kill previous timer and start new one (to "debounce").
  setTimeout delay delayedHandler >>= Just >>> flip Ref.write timerId


-- Adding a listener to `Debouncer` which will be called when a debouncer will
-- get a value and some gap of time is up after that (debounced).
subscribeToDebouncer
  :: forall a. Debouncer a -> (a -> Effect Unit) -> Effect DebouncerSubscription

subscribeToDebouncer (Debouncer { subscribers }) listener = do
  subscriberId <- newSubscriberId
  var <- AVar.empty -- new `AVar`

  -- Add a subscriber to subscribers list
  -- and start listening with provided subscriber.
  insert subscriberId var `Ref.modify_` subscribers

  launchAff_ $
    forever (AffAVar.take var >>= liftEffect <<< listener) `catchError` \e ->
      if Exception.message e /= unsubscribedExceptionMessage
         then throwError e
         else pure unit

  -- Construct subscription of subscriber identity and `unsubscribe` action.
  pure $ DebouncerSubscription subscriberId
       $ do delete subscriberId `Ref.modify_` subscribers
            Exception.error unsubscribedExceptionMessage `AVar.kill` var


-- Unsubscribe a listener from `Debouncer` so it will no longer be called by new
-- value sent to a debouncer.
unsubscribeFromDebouncer :: DebouncerSubscription -> Effect Unit
unsubscribeFromDebouncer (DebouncerSubscription _ unsubscribe) = unsubscribe


unsubscribedExceptionMessage :: String
unsubscribedExceptionMessage = "Unsubscribed from debouncer."
