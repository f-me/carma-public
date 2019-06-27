{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Carma.Utils.TypeSafe.Generic.DataType.Operations.MapConstructor
     ( type MapConstructorByName
     , type MapConstructorByName'
     ) where

import           GHC.Generics
import           GHC.TypeLits

import           Carma.Utils.TypeSafe.TypeFamilies


-- | Applies provided type-level function (type family) to a constructor
--   definition by that constructor's name.
--
-- For instance specified constructor (specified by its name) may contain record
-- fields, in this case provided type-level function will be applied to a
-- definition of those record fields.
type family MapConstructorByName
              (typeRep         :: * -> *)
              (constructorName :: Symbol)
              (f               :: (* -> *) -> (* -> *))
                               :: Maybe (* -> *)
                               where

  MapConstructorByName (D1 a b) constructorName f =
    FmapMaybe (D1 a) (MapConstructorByNameInternal b constructorName f)


-- | "MapConstructorByName" with type representation argument at the end.
type family MapConstructorByName'
              (constructorName :: Symbol)
              (f               :: (* -> *) -> (* -> *))
              (typeRep         :: * -> *)
                               :: Maybe (* -> *)
                               where

  MapConstructorByName' constructorName f (D1 a b) =
    FmapMaybe (D1 a) (MapConstructorByNameInternal b constructorName f)


type family MapConstructorByNameInternal
              (typeRep         :: * -> *)
              (constructorName :: Symbol)
              (f               :: (* -> *) -> (* -> *))
                               :: Maybe (* -> *)
                               where

  MapConstructorByNameInternal
    (C1 ('MetaCons constructorName a b) c) constructorName f =
      'Just (C1 ('MetaCons constructorName a b) (f c))

  MapConstructorByNameInternal (C1 ('MetaCons _ _ _) _) _ _ = 'Nothing

  MapConstructorByNameInternal
    (C1 ('MetaCons constructorName a b) c :+: xs) constructorName f =
      'Just (C1 ('MetaCons constructorName a b) (f c) :+: xs)

  MapConstructorByNameInternal
    (xs :+: C1 ('MetaCons constructorName a b) c) constructorName f =
      'Just (xs :+: C1 ('MetaCons constructorName a b) (f c))

  MapConstructorByNameInternal
    (xs :+: ys) constructorName f =
      MapConstructorByNameInternalIfEitherMaybe
        (MapConstructorByNameInternal xs constructorName f)
        (MapConstructorByNameInternal ys constructorName f)
        (
          (FromMaybe xs (MapConstructorByNameInternal xs constructorName f))
          :+:
          (FromMaybe ys (MapConstructorByNameInternal ys constructorName f))
        )


type family MapConstructorByNameInternalIfEitherMaybe
              (a                   :: Maybe (* -> *))
              (b                   :: Maybe (* -> *))
              (valueIfEitherIsJust :: (* -> *))
                                   :: Maybe (* -> *)
                                   where

  MapConstructorByNameInternalIfEitherMaybe ('Just _) 'Nothing x = 'Just x
  MapConstructorByNameInternalIfEitherMaybe 'Nothing ('Just _) x = 'Just x
  MapConstructorByNameInternalIfEitherMaybe 'Nothing 'Nothing  _ = 'Nothing
  MapConstructorByNameInternalIfEitherMaybe ('Just _) ('Just _) _ =
    TypeError ('Text "Datatype cannot have two constructors with the same name")
