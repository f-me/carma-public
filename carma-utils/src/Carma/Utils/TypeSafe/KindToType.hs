{-# LANGUAGE TypeInType #-}

module Carma.Utils.TypeSafe.KindToType
     ( kindToType
     ) where

import           Data.Proxy
import           Data.Kind


-- | Descend from kind-level to type-level.
kindToType :: Proxy (t :: k) -> Proxy (k :: *)
kindToType Proxy = Proxy
