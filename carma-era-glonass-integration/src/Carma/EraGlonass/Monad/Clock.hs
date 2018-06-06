-- An abstraction for monads which could get current time.
-- Useful for limiting possible monadic effects or mocking tests.
module Carma.EraGlonass.Monad.Clock
     ( MonadClock
     , getCurrentTime
     ) where

import qualified Data.Time.Clock (UTCTime, getCurrentTime)


class Monad m => MonadClock m where
  getCurrentTime :: m Data.Time.Clock.UTCTime

instance MonadClock IO where
  getCurrentTime = Data.Time.Clock.getCurrentTime
