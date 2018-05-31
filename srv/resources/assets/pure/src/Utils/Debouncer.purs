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

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Timer (TIMER, TimeoutId, setTimeout, clearTimeout)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, writeRef, modifyRef)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Aff (launchAff_)
import Control.Monad.Aff.AVar (AVAR, AVar, makeEmptyVar, putVar, takeVar)
import Control.Monad.Rec.Class (forever)

import Utils.SubscriberId (SubscriberId, newSubscriberId)


type DebouceTimeInMS = Int

newtype DebouncerSubscription = DebouncerSubscription SubscriberId
derive instance eqDebouncerSubscription  :: Eq DebouncerSubscription
derive instance ordDebouncerSubscription :: Ord DebouncerSubscription

data Debouncer a
   = Debouncer
   { delay       :: DebouceTimeInMS
   , timerId     :: Ref (Maybe TimeoutId)
   , subscribers :: Ref (Map DebouncerSubscription (AVar a))
   }


-- Creating new `Debouncer` reference with specified delay time in milliseconds.
-- A `Debouncer` will be ate by garbage collector, so you only need to
-- unsubscribe from it your listeners.
newDebouncer
  :: forall eff a. DebouceTimeInMS -> Eff (ref :: REF | eff) (Debouncer a)
newDebouncer delay = do
  timerId     <- newRef Nothing
  subscribers <- newRef empty
  pure $ Debouncer { delay, timerId, subscribers }


-- Send new value to `Debouncer`, this will debounce subscribers notifying with
-- specified delay (specified while creating new `Debouncer` by `newDebouncer`).
-- This will remove previous delayed trigger if it exists and will start again
-- waiting for whole delay time. After that time it will notify all subscribers.
sendToDebouncer
  :: forall eff a
   . Debouncer a
  -> a
  -> Eff (ref :: REF, timer :: TIMER, avar :: AVAR | eff) Unit

sendToDebouncer (Debouncer { delay, timerId, subscribers }) newValue = do
  readRef timerId
    >>= case _ of
             Nothing -> pure unit
             Just x  -> clearTimeout x

  let delayedHandler = do
        writeRef timerId Nothing
        readRef subscribers >>= traverse_ (launchAff_ <<< putVar newValue)

  setTimeout delay delayedHandler >>= Just >>> writeRef timerId


-- Adding a listener to `Debouncer` which will be called when a debouncer will
-- get a value and some gap of time is up after that (debounced).
subscribeToDebouncer
  :: forall eff a
   . Debouncer a
  -> (a -> Eff (ref :: REF, avar :: AVAR | eff) Unit)
  -> Eff ( ref    :: REF
         , avar   :: AVAR
         , now    :: NOW
         , random :: RANDOM
         | eff
         ) DebouncerSubscription

subscribeToDebouncer (Debouncer { subscribers }) listener = do
  subscription <- newSubscriberId <#> DebouncerSubscription

  launchAff_ $ do
    var <- makeEmptyVar
    liftEff $ subscribers `modifyRef` insert subscription var
    forever $ takeVar var >>= liftEff <<< unsafeCoerceEff <<< listener

  pure subscription


-- Unsubscribe a listener from `Debouncer` so it will no longer be called by new
-- value sent to a debouncer.
unsubscribeFromDebouncer
  :: forall eff a
   . Debouncer a
  -> DebouncerSubscription
  -> Eff (ref :: REF | eff) Unit

unsubscribeFromDebouncer (Debouncer { subscribers }) subscription =
  subscribers `modifyRef` delete subscription
