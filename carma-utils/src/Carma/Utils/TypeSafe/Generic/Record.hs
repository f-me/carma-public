{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Carma.Utils.TypeSafe.Generic.Record
     ( FieldName
     , fieldName
     , fieldName'
     , ConstructorFieldName
     , constructorFieldName
     , constructorFieldName'
     , FieldType
     , fieldType
     , fieldType'
     , ConstructorFieldType
     , constructorFieldType
     , constructorFieldType'

     , ReplaceFieldKey
     ) where

import           GHC.Generics
import           GHC.TypeLits

import           Data.Proxy
import           Data.String (IsString (fromString))

import           Carma.Utils.TypeSafe.TypeFamilies


-- | Helps to use a field name of a record type with protection of its
-- correctness.
type family FieldName (k1 :: * -> *) (k2 :: Symbol) :: Maybe Symbol where
  FieldName (D1 _ (C1 ('MetaCons _ _ _) x)) s = RecordFieldName x s
  FieldName (D1 a (C1 ('MetaCons _ _ _) x :+: xs)) s =
    MaybeAlternative (RecordFieldName x s) (FieldName (D1 a xs) s)
  FieldName (D1 a (xs :+: ys)) s =
    MaybeAlternative (FieldName (D1 a xs) s) (FieldName (D1 a ys) s)
  FieldName (D1 _ _) _ = 'Nothing

-- | Helps to use field name of a proxied record type with protection of its
-- correctness.
fieldName
  :: forall t typeRep field str
  .  ( typeRep ~ Rep t
     , FieldName typeRep field ~ 'Just field
     , KnownSymbol field
     , IsString str
     )
  => Proxy '(t, field)
  -> str

fieldName Proxy = fieldName' (Proxy :: Proxy '(typeRep, field))

-- | Alternative version of "fieldName" which presumes that provided type is
-- already a @(Rep ofSomething)@.
--
-- Could be useful in cases when @Rep ofType@ is modified.
fieldName'
  :: forall typeRep field str
  .  ( FieldName typeRep field ~ 'Just field
     , KnownSymbol field
     , IsString str
     )
  => Proxy '(typeRep, field)
  -> str

fieldName' Proxy = fromString $ symbolVal (Proxy :: Proxy field)

-- | Helps to use a field name of a record type with protection of its
-- correctness and that this field defined inside provided constructor.
--
-- It will return @Nothing@ if neither there's no such constructor for provided
-- type nor if such field name not found inside definition of that constructor.
type family ConstructorFieldName (k1 :: * -> *)
                                 (k2 :: Symbol)
                                 (k3 :: Symbol)
                                     :: Maybe Symbol where
  ConstructorFieldName (D1 _ (C1 ('MetaCons c _ 'True) x)) c f =
    RecordFieldName x f
  ConstructorFieldName (D1 a (C1 ('MetaCons c _ 'True) x :+: _)) c f =
    RecordFieldName x f
  ConstructorFieldName (D1 a (C1 ('MetaCons _ _ _) _ :+: xs)) c f =
    ConstructorFieldName (D1 a xs) c f
  ConstructorFieldName (D1 a (xs :+: ys)) c f =
    MaybeAlternative
      (ConstructorFieldName (D1 a xs) c f)
      (ConstructorFieldName (D1 a ys) c f)
  ConstructorFieldName (D1 _ _) _ _ = 'Nothing

-- | Helps to use a field name of a proxied record type with protection of its
-- correctness and that this field defined inside provided constructor.
constructorFieldName
  :: forall t typeRep constructor field str
  .  ( typeRep ~ Rep t
     , ConstructorFieldName typeRep constructor field ~ 'Just field
     , KnownSymbol field
     , IsString str
     )
  => Proxy '(t, constructor, field)
  -> str

constructorFieldName Proxy =
  constructorFieldName' (Proxy :: Proxy '(typeRep, constructor, field))

-- | Alternative version of "constructorFieldName" which presumes that provided
-- type is already a @(Rep ofSomething)@.
--
-- Could be useful in cases when @Rep ofType@ is modified.
constructorFieldName'
  :: forall typeRep constructor field str
  .  ( ConstructorFieldName typeRep constructor field ~ 'Just field
     , KnownSymbol field
     , IsString str
     )
  => Proxy '(typeRep, constructor, field)
  -> str

constructorFieldName' Proxy = fromString $ symbolVal (Proxy :: Proxy field)


-- | Helps to use a field type of a record type with protection of its
-- correctness (you couldn't obtain a type by name of a field which isn't
-- defined).
type family FieldType (k1 :: * -> *) (k2 :: Symbol) :: Maybe * where
  FieldType (D1 _ (C1 ('MetaCons _ _ 'True) x)) s = RecordFieldType x s
  FieldType (D1 a (C1 ('MetaCons _ _ 'True) x :+: xs)) s =
    MaybeAlternative (RecordFieldType x s) (FieldType (D1 a xs) s)
  FieldType (D1 a (xs :+: ys)) s =
    MaybeAlternative (FieldType (D1 a xs) s) (FieldType (D1 a ys) s)
  FieldType (D1 _ _) _ = 'Nothing

-- | Helps to use a field type of a proxied record type with protection of its
-- correctness (you couldn't obtain a type by name of a field which isn't
-- defined).
fieldType
  :: ( typeRep ~ Rep t
     , FieldType typeRep fieldName ~ 'Just fieldType
     )
  => Proxy '(t, fieldName)
  -> Proxy fieldType

fieldType Proxy = Proxy

-- | Alternative version of "fieldType" which presumes that provided type is
-- already a @(Rep ofSomething)@.
--
-- Could be useful in cases when @Rep ofType@ is modified.
fieldType'
  :: (FieldType typeRep fieldName ~ 'Just fieldType)
  => Proxy '(typeRep, fieldName)
  -> Proxy fieldType

fieldType' Proxy = Proxy

-- | Helps to use a field type of a record type with protection of its
-- correctness and that this field defined inside provided constructor
-- (you couldn't obtain a type by name of a field which isn't defined).
--
-- It will return @Nothing@ if neither there's no such constructor for provided
-- type nor if such field name not found inside definition of that constructor.
type family ConstructorFieldType (k1 :: * -> *)
                                 (k2 :: Symbol)
                                 (k3 :: Symbol)
                                     :: Maybe * where
  ConstructorFieldType (D1 _ (C1 ('MetaCons c _ 'True) x)) c f =
    RecordFieldType x f
  ConstructorFieldType (D1 a (C1 ('MetaCons c _ 'True) x :+: _)) c f =
    RecordFieldType x f
  ConstructorFieldType (D1 a (C1 ('MetaCons _ _ _) _ :+: xs)) c f =
    ConstructorFieldType (D1 a xs) c f
  ConstructorFieldType (D1 a (xs :+: ys)) c f =
    MaybeAlternative
      (ConstructorFieldType (D1 a xs) c f)
      (ConstructorFieldType (D1 a ys) c f)
  ConstructorFieldType (D1 _ _) _ _ = 'Nothing

-- | Helps to use a field type of a proxied record type with protection of its
-- correctness and that this field defined inside provided constructor
-- (you couldn't obtain a type by name of a field which isn't defined).
constructorFieldType
  :: ( typeRep ~ Rep t
     , ConstructorFieldType typeRep constructorName fieldName
         ~ 'Just fieldType
     )
  => Proxy '(t, constructorName, fieldName)
  -> Proxy fieldType

constructorFieldType Proxy = Proxy

-- | Alternative version of "constructorFieldType" which presumes that provided
-- type is already a @(Rep ofSomething)@.
--
-- Could be useful in cases when @Rep ofType@ is modified.
constructorFieldType'
  :: ( ConstructorFieldType typeRep constructorName fieldName
         ~ 'Just fieldType
     )
  => Proxy '(typeRep, constructorName, fieldName)
  -> Proxy fieldType

constructorFieldType' Proxy = Proxy


-- | Sub-helper of @FieldName@ and @ConstructorName@ to iterate over fields
-- definitions inside a constructor.
type family RecordFieldName (k1 :: * -> *) (k2 :: Symbol) :: Maybe Symbol where
  RecordFieldName (S1 ('MetaSel ('Just x) _ _ _) _) x = 'Just x
  RecordFieldName (S1 ('MetaSel ('Just x) _ _ _) _ :*: _) x = 'Just x
  RecordFieldName (S1 ('MetaSel _ _ _ _) _ :*: xs) x = RecordFieldName xs x
  RecordFieldName (xs :*: ys) x =
    MaybeAlternative (RecordFieldName xs x) (RecordFieldName ys x)
  RecordFieldName (S1 _ _) _ = 'Nothing

-- | Sub-helper of @FieldType@ and @ConstructorFieldType@ to iterate over fields
-- definitions inside a constructor.
type family RecordFieldType (k1 :: * -> *) (k2 :: Symbol) :: Maybe * where
  RecordFieldType (S1 ('MetaSel ('Just x) _ _ _) (Rec0 t)) x = 'Just t
  RecordFieldType (S1 ('MetaSel ('Just x) _ _ _) (Rec0 t) :*: _) x = 'Just t
  RecordFieldType (S1 ('MetaSel _ _ _ _) _ :*: xs) x = RecordFieldType xs x
  RecordFieldType (xs :*: ys) x =
    MaybeAlternative (RecordFieldType xs x) (RecordFieldType ys x)
  RecordFieldType (S1 _ _) _ = 'Nothing


-- | Helps to rename a field of a record
type family ReplaceFieldKey (from :: Symbol) (to :: Symbol) field where
  ReplaceFieldKey from to (S1 ('MetaSel ('Just from) a b c) d) =
    S1 ('MetaSel ('Just to) a b c) d
