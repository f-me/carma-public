{-# LANGUAGE DataKinds, PolyKinds #-}

-- | Some helpers for "Proxy" data-type.
module Carma.Utils.TypeSafe.Proxy
     ( proxyPair
     , proxyTriplet
     , proxyQuartet
     , proxyPair2Triplet
     , proxyTriplet2Quartet
     ) where

import           Data.Proxy


proxyPair :: Proxy a -> Proxy b -> Proxy '(a, b)
proxyPair Proxy Proxy = Proxy


proxyTriplet :: Proxy a -> Proxy b -> Proxy c -> Proxy '(a, b, c)
proxyTriplet Proxy Proxy Proxy = Proxy


proxyQuartet :: Proxy a -> Proxy b -> Proxy c -> Proxy d -> Proxy '(a, b, c, d)
proxyQuartet Proxy Proxy Proxy Proxy = Proxy


proxyPair2Triplet :: Proxy '(a, b) -> Proxy c -> Proxy '(a, b, c)
proxyPair2Triplet Proxy Proxy = Proxy


proxyTriplet2Quartet :: Proxy '(a, b, c) -> Proxy d -> Proxy '(a, b, c, d)
proxyTriplet2Quartet Proxy Proxy = Proxy
