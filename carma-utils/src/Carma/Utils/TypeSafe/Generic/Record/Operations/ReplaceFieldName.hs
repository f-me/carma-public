{-# LANGUAGE DataKinds, TypeFamilies, ExplicitNamespaces #-}

module Carma.Utils.TypeSafe.Generic.Record.Operations.ReplaceFieldName
     ( type ReplaceFieldName
     ) where

import           GHC.Generics
import           GHC.TypeLits


-- | Helps to rename a field of a record
type family ReplaceFieldName (from :: Symbol) (to :: Symbol) field where
  ReplaceFieldName from to (S1 ('MetaSel ('Just from) a b c) d) =
    S1 ('MetaSel ('Just to) a b c) d
