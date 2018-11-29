{-# LANGUAGE DataKinds, PolyKinds #-}

-- | Some helpers for "Proxy" data-type.
module Carma.Utils.TypeSafe.Proxy
     ( proxyPair
     , proxyTriplet
     , proxyPair2Triplet
     ) where

import           Data.Proxy


proxyPair :: Proxy a -> Proxy b -> Proxy '(a, b)
proxyPair Proxy Proxy = Proxy


proxyTriplet :: Proxy a -> Proxy b -> Proxy c -> Proxy '(a, b, c)
proxyTriplet Proxy Proxy Proxy = Proxy


proxyPair2Triplet :: Proxy '(a, b) -> Proxy c -> Proxy '(a, b, c)
proxyPair2Triplet Proxy Proxy = Proxy
