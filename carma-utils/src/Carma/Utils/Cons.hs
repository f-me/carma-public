{-# LANGUAGE PatternSynonyms, ConstraintKinds #-}

module Carma.Utils.Cons
     ( module Control.Lens.Cons
     , Cons'
     , Snoc'
     ) where

import           Control.Lens.Cons
                   ( Cons, Snoc
                   , (<|), (|>)
                   , cons, snoc
                   , uncons, unsnoc
                   , pattern (:<), pattern (:>)
                   )


-- | Reduces noiseness in "Control.Lens.Cons.Cons" constraints.
type Cons' f a = Cons (f a) (f a) a a


-- | Reduces noiseness in "Control.Lens.Cons.Snoc" constraints.
type Snoc' f a = Snoc (f a) (f a) a a
