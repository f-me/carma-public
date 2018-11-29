{-# LANGUAGE DataKinds, KindSignatures, ScopedTypeVariables #-}

-- | Like "GHC.TypeLits.KnownSymbol" but for "Bool"s.
module Carma.Utils.TypeSafe.KnownBool
     ( KnownBool (..)
     ) where


class KnownBool (b :: Bool) where boolVal :: forall proxy . proxy b -> Bool
instance KnownBool 'True    where boolVal _ = True
instance KnownBool 'False   where boolVal _ = False
