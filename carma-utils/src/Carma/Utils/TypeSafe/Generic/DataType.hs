{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Carma.Utils.TypeSafe.Generic.DataType
     ( TypeName
     , typeName
     , typeName'
     , ConstructorName
     , constructorName
     , constructorName'
     ) where

import           GHC.Generics
import           GHC.TypeLits

import           Data.Proxy
import           Data.String (IsString (fromString))

import           Carma.Utils.TypeSafe.TypeFamilies


-- | Helps to extract type name from "Generic" instance.
type family TypeName (k1 :: * -> *) :: Symbol where
  TypeName (D1 ('MetaData name _ _ _) _) = name

-- | Helps to extract type name from "Generic" instance.
--
-- To any string type just by using type proxy.
typeName
  :: forall t typeRep typeName str
  .  ( typeRep ~ Rep t
     , typeName ~ TypeName (Rep t)
     , KnownSymbol typeName
     , IsString str
     )
  => Proxy t
  -> str

typeName Proxy = typeName' (Proxy :: Proxy typeRep)

-- | Alternative version of "typeName" which presumes that provided type is
-- already a @(Rep ofSomething)@.
--
-- Could be useful in cases when @Rep ofType@ is modified.
typeName'
  :: forall typeRep typeName str
  . ( typeName ~ TypeName typeRep
    , KnownSymbol typeName
    , IsString str
    )
  => Proxy typeRep
  -> str

typeName' Proxy = fromString $ symbolVal (Proxy :: Proxy typeName)


-- | Helps to use constructor name of a type with protection of its correctness.
type family ConstructorName (k1 :: * -> *) (k2 :: Symbol) :: Maybe Symbol where
  ConstructorName (D1 _ (C1 ('MetaCons x _ _) _)) x = 'Just x
  ConstructorName (D1 _ (C1 ('MetaCons x _ _) _ :+: _)) x = 'Just x
  ConstructorName (D1 a (C1 _ _ :+: xs)) x = ConstructorName (D1 a xs) x
  ConstructorName (D1 a (xs :+: ys)) x =
    MaybeAlternative (ConstructorName (D1 a xs) x) (ConstructorName (D1 a ys) x)
  ConstructorName (D1 _ _) _ = 'Nothing

-- | Helps to use constructor name of a proxied type with protection of its
-- correctness.
constructorName
  :: forall t typeRep constructor str
  .  ( typeRep ~ Rep t
     , ConstructorName typeRep constructor ~ 'Just constructor
     , KnownSymbol constructor
     , IsString str
     )
  => Proxy '(t, constructor)
  -> str

constructorName Proxy = fromString $ symbolVal (Proxy :: Proxy constructor)

-- | Alternative version of "constructorName" which presumes that provided type
-- is already a @(Rep ofSomething)@.
--
-- Could be useful in cases when @Rep ofType@ is modified.
constructorName'
  :: forall typeRep constructor str
  .  ( ConstructorName typeRep constructor ~ 'Just constructor
     , KnownSymbol constructor
     , IsString str
     )
  => Proxy '(typeRep, constructor)
  -> str

constructorName' Proxy = fromString $ symbolVal (Proxy :: Proxy constructor)
