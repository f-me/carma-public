module Carma.Utils.Operators
     ( (?), (<&>), (<&!>)
     , module Control.Monad
     ) where

import           Control.Monad ((<$!>))


(?) :: (a -> b) -> (b -> c) -> (a -> c)
(?) = flip (.)
{-# INLINE (?) #-}
infixl 9 ?

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
{-# INLINE (<&>) #-}
infixr 5 <&>

(<&!>) :: Monad m => m a -> (a -> b) -> m b
(<&!>) = flip (<$!>)
{-# INLINE (<&!>) #-}
infixr 5 <&!>
