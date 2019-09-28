module Carma.Utils
     ( module Carma.Utils.Operators
     , module Carma.Utils.Numbers
     , possibly
     ) where

import           Carma.Utils.Operators
import           Carma.Utils.Numbers


-- | Left-to-right shorthand for @maybe (pure ()) action x@ pattern.
--
-- Do something with a value from @Just@ if we have it, if it isn't @Nothing@.
--
-- This:
--
-- @
-- let someValue = Just 10
-- maybe (pure ()) print someValue
-- @
--
-- Becomes this:
--
-- @
-- let someValue = Just 10
-- someValue `possibly` print
-- @
possibly :: Applicative f => Maybe a -> (a -> f ()) -> f ()
possibly = flip $ maybe $ pure ()
{-# INLINE possibly #-}
