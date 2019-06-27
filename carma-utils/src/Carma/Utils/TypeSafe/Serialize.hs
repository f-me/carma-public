{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, ScopedTypeVariables #-}

module Carma.Utils.TypeSafe.Serialize
     ( SerializableListOfKnownSymbols (..)
     ) where

import           GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

import           Data.Proxy
import           Data.String (IsString (fromString))


class SerializableListOfKnownSymbols (a :: [Symbol]) where
  -- | Can be serialized to @[String]@ or @Set Text@, etc.
  serializeListOfKnownSymbols
    :: (Applicative m, Monoid (m s), IsString s) => Proxy a -> m s

instance SerializableListOfKnownSymbols '[] where
  serializeListOfKnownSymbols Proxy = mempty

instance (KnownSymbol x, SerializableListOfKnownSymbols xs)
      => SerializableListOfKnownSymbols (x ': xs)
         where
  serializeListOfKnownSymbols Proxy =
    pure (fromString (symbolVal (Proxy :: Proxy x))) `mappend`
    serializeListOfKnownSymbols (Proxy :: Proxy xs)
