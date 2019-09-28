module Carma.Utils.Operators
     ( (!), (?), (<&>), (<&!>), (<<)
     , module Data.Function
     , module Control.Monad
     ) where

import           Data.Function ((&))
import           Control.Monad ((<$!>))


(!) :: a -> b -> b
(!) = seq
{-# INLINE (!) #-}
infixr 0 !


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

(<<) :: Monad m => m a -> m b -> m a
(<<) = flip (>>)
{-# INLINE (<<) #-}
infixr 1 <<
