{-# LANGUAGE TypeInType #-}

module Carma.Utils.TypeSafe.KindToType
     ( kindToType
     ) where

import           Data.Proxy


-- | Descend from kind-level to type-level.
kindToType :: Proxy (t :: k) -> Proxy (k :: *)
kindToType Proxy = Proxy
