{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

-- | Helpers for building Aeson instances based on @StringyEnum@.
module Carma.Utils.StringyEnum.Aeson
     ( parseStringyEnumJSON
     ) where

import           GHC.Generics
import           GHC.TypeLits

import           Data.Proxy
import           Data.Aeson
import           Data.Aeson.Types (Parser, typeMismatch)

import           Carma.Utils.TypeSafe.Generic.DataType


-- | Producing list of all values to reduce human-factor mistakes,
--   so it is handled automatically when we add a new value.
parseStringyEnumJSON
  :: forall t
   . ( ToJSON t
     , Enum t
     , Bounded t
     , Generic t
     , KnownSymbol (TypeName (Rep t))
     )
  => Value
  -> Parser t

parseStringyEnumJSON jsonValue = f [minBound..maxBound :: t] where
  f [] = typeMismatch (typeName (Proxy :: Proxy t)) jsonValue
  f (x:xs) | toJSON x == jsonValue = pure x
           | otherwise             = f xs
