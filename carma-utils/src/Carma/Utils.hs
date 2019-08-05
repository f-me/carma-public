module Carma.Utils
     ( module Carma.Utils.Operators
     , module Carma.Utils.Numbers
     , possibly
     ) where

import           Carma.Utils.Operators
import           Carma.Utils.Numbers


possibly :: Monad m => Maybe a -> (a -> m ()) -> m ()
possibly = flip $ maybe $ pure ()
{-# INLINE possibly #-}
