{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, ExplicitNamespaces #-}
{-# LANGUAGE UndecidableInstances #-}

module Carma.Utils.TypeSafe.Generic.DataType.Operations.RemoveConstructor
     ( type RemoveConstructorByName
     , type RemoveConstructorByName'
     ) where

import           GHC.Generics
import           GHC.TypeLits

import           Carma.Utils.TypeSafe.TypeFamilies


type family RemoveConstructorByName
  (typeRep         :: * -> *)
  (constructorName :: Symbol)
                   :: Maybe (* -> *)
                   where

  RemoveConstructorByName (D1 a x) constructorName =
    FmapMaybe (D1 a) (RemoveConstructorByNameInternal x constructorName)

-- | "RemoveConstructorByName" with type representation argument at the end.
type family RemoveConstructorByName'
  (constructorName :: Symbol)
  (typeRep         :: * -> *)
                   :: Maybe (* -> *)
                   where

  RemoveConstructorByName' constructorName (D1 a x) =
    FmapMaybe (D1 a) (RemoveConstructorByNameInternal x constructorName)


type family RemoveConstructorByNameInternal
  (constructorsRep :: * -> *)
  (constructorName :: Symbol)
                   :: Maybe (* -> *)
                   where

  RemoveConstructorByNameInternal
    (C1 ('MetaCons constructorName _ _) _) constructorName =
      'Just V1

  RemoveConstructorByNameInternal (C1 ('MetaCons _ _ _) _) _ = 'Nothing

  RemoveConstructorByNameInternal
    (C1 ('MetaCons constructorName _ _) _ :+: xs) constructorName =
      'Just xs

  RemoveConstructorByNameInternal
    (xs :+: C1 ('MetaCons constructorName _ _) _) constructorName =
      'Just xs

  RemoveConstructorByNameInternal
    (xs :+: ys) constructorName =
      RemoveConstructorByNameInternalIfEitherMaybe
        (RemoveConstructorByNameInternal xs constructorName)
        (RemoveConstructorByNameInternal ys constructorName)
        (
          (FromMaybe xs (RemoveConstructorByNameInternal xs constructorName))
          :+:
          (FromMaybe ys (RemoveConstructorByNameInternal ys constructorName))
        )


type family RemoveConstructorByNameInternalIfEitherMaybe
  (a                   :: Maybe (* -> *))
  (b                   :: Maybe (* -> *))
  (valueIfEitherIsJust :: (* -> *))
                       :: Maybe (* -> *)
                       where

  RemoveConstructorByNameInternalIfEitherMaybe ('Just _) 'Nothing x = 'Just x
  RemoveConstructorByNameInternalIfEitherMaybe 'Nothing ('Just _) x = 'Just x
  RemoveConstructorByNameInternalIfEitherMaybe 'Nothing 'Nothing  _ = 'Nothing
  RemoveConstructorByNameInternalIfEitherMaybe ('Just _) ('Just _) _ =
    TypeError ('Text "Datatype cannot have two constructors with the same name")
