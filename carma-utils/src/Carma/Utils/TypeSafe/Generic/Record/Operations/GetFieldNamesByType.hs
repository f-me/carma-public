{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# LANGUAGE ExplicitNamespaces, ScopedTypeVariables #-}

module Carma.Utils.TypeSafe.Generic.Record.Operations.GetFieldNamesByType
     ( type GetFieldNamesByType
     , type GetFieldNamesByType'
     , getFieldNamesByType
     , getFieldNamesByType'

     , type GetFieldNamesOfConstructorByType
     , type GetFieldNamesOfConstructorByType'
     , getFieldNamesOfConstructorByType
     , getFieldNamesOfConstructorByType'
     ) where

import           GHC.Generics
import           GHC.TypeLits

import           Data.Proxy
import           Data.String (type IsString)

import           Carma.Utils.TypeSafe.TypeFamilies (type Concat, type Reverse)

import           Carma.Utils.TypeSafe.Serialize
                   ( type SerializableListOfKnownSymbols (..)
                   )


-- | Retrieves all field names from all constructors which matches provided
--   type from type Generic representation.
type family GetFieldNamesByType (k1 :: * -> *) (k2 :: *) :: [Symbol] where
  GetFieldNamesByType (D1 _ b) t = Reverse (GetFieldNamesByTypeInternal b t)

-- | Version of "GetFieldNamesByType" with moved
--   type representation argument to the end.
type family GetFieldNamesByType' (k1 :: *) (k2 :: * -> *) :: [Symbol] where
  GetFieldNamesByType' t (D1 _ b) = Reverse (GetFieldNamesByTypeInternal b t)

-- | See "GetFieldNamesByType".
type family GetFieldNamesByTypeInternal (k1 :: * -> *) (k2 :: *) :: [Symbol]
            where

  GetFieldNamesByTypeInternal (C1 ('MetaCons _ _ 'True) x) t =
    GetFieldNamesByTypeInternal x t

  -- Skipping constructors with no record fields
  GetFieldNamesByTypeInternal (C1 ('MetaCons _ _ 'False) _) _ = '[]

  GetFieldNamesByTypeInternal (xs :+: ys) t =
    Concat (GetFieldNamesByTypeInternal xs t) (GetFieldNamesByTypeInternal ys t)

  GetFieldNamesByTypeInternal (S1 ('MetaSel ('Just n) _ _ _) (Rec0 t)) t = '[n]

  GetFieldNamesByTypeInternal (S1 ('MetaSel _ _ _ _) _) _ = '[]

  GetFieldNamesByTypeInternal (xs :*: ys) t =
    Concat (GetFieldNamesByTypeInternal xs t) (GetFieldNamesByTypeInternal ys t)


-- | Version of "GetFieldNamesByType"
--   but with additional constructor name constraint.
type family GetFieldNamesOfConstructorByType (k1 :: * -> *)
                                             (k2 :: Symbol)
                                             (k3 :: *)
                                                 :: [Symbol]
                                                 where

  GetFieldNamesOfConstructorByType (D1 a b) c t =
    Reverse (
      GetFieldNamesOfConstructorByTypeInternalResolve (D1 a b) c t
        (GetFieldNamesOfConstructorByTypeInternal b c t)
    )

-- | Version of "GetFieldNamesOfConstructorByType" with moved
--   type representation argument to the end.
type family GetFieldNamesOfConstructorByType' (k1 :: Symbol)
                                              (k2 :: *)
                                              (k3 :: * -> *)
                                                  :: [Symbol]
                                                  where

  GetFieldNamesOfConstructorByType' c t (D1 a b) =
    Reverse (
      GetFieldNamesOfConstructorByTypeInternalResolve (D1 a b) c t
        (GetFieldNamesOfConstructorByTypeInternal b c t)
    )

-- | See "GetFieldNamesOfConstructorByType".
type family GetFieldNamesOfConstructorByTypeInternal (k1 :: * -> *)
                                                     (k2 :: Symbol)
                                                     (k3 :: *)
                                                         :: (Bool, [Symbol])
                                                          where

  GetFieldNamesOfConstructorByTypeInternal (C1 ('MetaCons c _ 'True) x) c t =
    GetFieldNamesOfConstructorByTypeInternalConcat
      '( 'True, '[] ) (GetFieldNamesOfConstructorByTypeInternal x c t)

  -- Skipping constructors with no record fields
  -- and not matched constructor name.
  GetFieldNamesOfConstructorByTypeInternal (C1 ('MetaCons _ _ _) _) _ _ =
    '( 'False, '[] )

  GetFieldNamesOfConstructorByTypeInternal (xs :+: ys) c t =
    GetFieldNamesOfConstructorByTypeInternalConcat
      (GetFieldNamesOfConstructorByTypeInternal xs c t)
      (GetFieldNamesOfConstructorByTypeInternal ys c t)

  GetFieldNamesOfConstructorByTypeInternal
    (S1 ('MetaSel ('Just n) _ _ _) (Rec0 t)) _ t = '( 'True, '[n] )

  GetFieldNamesOfConstructorByTypeInternal
    (S1 ('MetaSel _ _ _ _) _) _ _ = '( 'True, '[] )

  GetFieldNamesOfConstructorByTypeInternal (xs :*: ys) c t =
    GetFieldNamesOfConstructorByTypeInternalConcat
      (GetFieldNamesOfConstructorByTypeInternal xs c t)
      (GetFieldNamesOfConstructorByTypeInternal ys c t)

-- | Helps to indicate whether provided constructor have been found at all.
type family GetFieldNamesOfConstructorByTypeInternalConcat
            (k1 :: (Bool, [Symbol]))
            (k2 :: (Bool, [Symbol]))
                :: (Bool, [Symbol])
                where
  GetFieldNamesOfConstructorByTypeInternalConcat '( 'True, xs ) '( _,     ys ) =
    '( 'True,  Concat xs ys )
  GetFieldNamesOfConstructorByTypeInternalConcat '( _,     xs ) '( 'True, ys ) =
    '( 'True,  Concat xs ys )
  GetFieldNamesOfConstructorByTypeInternalConcat '( _,     xs ) '( _,     ys ) =
    '( 'False, Concat xs ys )

type family GetFieldNamesOfConstructorByTypeInternalResolve
            (k1 :: * -> *)
            (k2 :: Symbol)
            (k3 :: *)
            (k4 :: (Bool, [Symbol]))
                :: [Symbol]
                where
  GetFieldNamesOfConstructorByTypeInternalResolve _ _ _ '( 'True, xs ) = xs
  GetFieldNamesOfConstructorByTypeInternalResolve typeRep c fieldType _ =
    TypeError (
      'Text "Collecting field names by " ':<>: 'ShowType fieldType ':<>:
      'Text " type is failed due to constructor " ':<>: 'ShowType c ':<>:
      'Text " not found in " ':<>: 'ShowType typeRep
    )


-- | Serialize type-level list of field names to term-level (runtime level).
getFieldNamesByType
  :: forall type' typeRep fieldType m s list
   .
   ( IsString s
   , Applicative m
   , Monoid (m s)
   , typeRep ~ Rep type'
   , SerializableListOfKnownSymbols list
   , list ~ GetFieldNamesByType typeRep fieldType
   )
  => Proxy '(type', fieldType)
  -> m s

getFieldNamesByType Proxy = serializeListOfKnownSymbols (Proxy :: Proxy list)


-- | Version of "getFieldNamesByType"
--   but with additional constructor name constraint.
getFieldNamesOfConstructorByType
  :: forall type' typeRep constructorName fieldType m s list
   .
   ( IsString s
   , Applicative m
   , Monoid (m s)
   , typeRep ~ Rep type'
   , SerializableListOfKnownSymbols list
   , list ~ GetFieldNamesOfConstructorByType typeRep constructorName fieldType
   )
  => Proxy '(type', constructorName, fieldType)
  -> m s

getFieldNamesOfConstructorByType Proxy =
  serializeListOfKnownSymbols (Proxy :: Proxy list)


-- | Version of "getFieldNamesByType"
--   which operates directly on type representation.
getFieldNamesByType'
  :: forall typeRep fieldType m s list
   .
   ( IsString s
   , Applicative m
   , Monoid (m s)
   , SerializableListOfKnownSymbols list
   , list ~ GetFieldNamesByType typeRep fieldType
   )
  => Proxy '(typeRep, fieldType)
  -> m s

getFieldNamesByType' Proxy = serializeListOfKnownSymbols (Proxy :: Proxy list)

-- | Version of "getFieldNamesOfConstructorByType"
--   which operates directly on type representation.
getFieldNamesOfConstructorByType'
  :: forall typeRep constructorName fieldType m s list
   .
   ( IsString s
   , Applicative m
   , Monoid (m s)
   , SerializableListOfKnownSymbols list
   , list ~ GetFieldNamesOfConstructorByType typeRep constructorName fieldType
   )
  => Proxy '(typeRep, constructorName, fieldType)
  -> m s

getFieldNamesOfConstructorByType' Proxy =
  serializeListOfKnownSymbols (Proxy :: Proxy list)
