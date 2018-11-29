{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Helpers for building/defining Era Glonass types.
--
-- TODO particular sets of helpers could be separated to own modules,
--      this one is pretty fat already.
--
-- TODO some of helpers here could be generic to whole project not just this
--      particular branch of modules hierarchy, maybe move most of them to the
--      @carma-utils@ package?
--
module Carma.EraGlonass.Types.Helpers
     ( ReplaceFieldKey

     , TypeName
     , typeName
     , typeName'
     , ConstructorName
     , constructorName
     , constructorName'
     , FieldName
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

     , addConstructorTag
     , addConstructorTag'
     , removeConstructorTag

     , TypeSafeSchemaProperty
     , typeSafeSchemaProperty
     , typeSafeSchemaProperty'
     , TypeSafeSchemaProperties
     , typeSafeSchemaProperties
     , typeSafeSchemaProperties'
     , typeSafeSchemaSeparatedProperties
     , typeSafeSchemaSeparatedProperties'
     , TypeSafeSchemaConstructorsAsProperties
     , typeSafeSchemaConstructorsAsProperties
     , typeSafeSchemaConstructorsAsProperties'
     , typeSafeSchemaMapConstructorsAsProperties
     , typeSafeSchemaMapConstructorsAsProperties'

     , constructorsBranchingSchemaProto
     ) where

import           GHC.Generics
import           GHC.TypeLits
import           GHC.Exts (IsList (..))

import           Data.Proxy
import qualified Data.HashMap.Lazy as HM
import           Data.String (IsString (fromString))
import           Data.Aeson
import           Data.Swagger
import           Data.Swagger.Declare

import           Carma.Utils.Operators
import           Carma.Utils.TypeSafe.KnownBool


-- | Helps to rename a field of a record
type family ReplaceFieldKey (from :: Symbol) (to :: Symbol) field where
  ReplaceFieldKey from to (S1 ('MetaSel ('Just from) a b c) d) =
    S1 ('MetaSel ('Just to) a b c) d


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


-- | Helps to build custom @FromJSON@ instances.
--
-- For example when you have different set of required fields in raw JSON data
-- depending on some field. So you're looking at that field first and then
-- decide which constructor to use by adding a "tag" field to raw JSON data to
-- give that extended raw JSON to generic @FromJSON@ implementation. It could be
-- a separation between successful and failure cases (two constructors).
--
-- Also this function is type-level protected, so if you passed a correct
-- proxied type with constructors, which one of them you're adding here, you
-- can't compile until you set correct name of one.
addConstructorTag
  :: forall t typeRep constructor
  .  ( typeRep ~ Rep t
     , ConstructorName typeRep constructor ~ 'Just constructor
     , KnownSymbol constructor
     )
  => Proxy '(t, constructor)
  -> Object
  -> Object

addConstructorTag Proxy =
  addConstructorTag' (Proxy :: Proxy '(typeRep, constructor))

-- | Alternative version of "addConstructorTag'" which presumes that provided
-- type is already a @(Rep ofSomething)@.
--
-- Could be useful in cases when @Rep ofType@ is modified.
addConstructorTag'
  :: ( ConstructorName typeRep constructor ~ 'Just constructor
     , KnownSymbol constructor
     )
  => Proxy '(typeRep, constructor)
  -> Object
  -> Object

addConstructorTag' p@Proxy = HM.insert "tag" $ String $ constructorName' p

-- | Helps to build custom @ToJSON@ instances avoiding injecting custom
-- implementation artefacts to the result output.
removeConstructorTag :: Object -> Object
removeConstructorTag = HM.delete "tag"


type family MaybeAlternative (k1 :: Maybe k)
                             (k2 :: Maybe k)
                                 :: Maybe k where
  MaybeAlternative ('Just x) _        = 'Just x
  MaybeAlternative 'Nothing ('Just x) = 'Just x
  MaybeAlternative 'Nothing 'Nothing  = 'Nothing


-- | Kinda like "Applicative" stuff
type family MaybePair (k1 :: Maybe kk1)
                      (k2 :: Maybe kk2)
                          :: Maybe (kk1, kk2) where
  MaybePair ('Just a) ('Just b) = 'Just '(a, b)
  MaybePair _ _ = 'Nothing

-- | Kinda like "Applicative" stuff
type family MaybeTriplet (k1 :: Maybe kk1)
                         (k2 :: Maybe kk2)
                         (k3 :: Maybe kk3)
                             :: Maybe (kk1, kk2, kk3) where
  MaybeTriplet ('Just a) ('Just b) ('Just c) = 'Just '(a, b, c)
  MaybeTriplet _ _ _ = 'Nothing


type family TypeIsMaybe (k1 :: *) :: Bool where
  TypeIsMaybe (Maybe _) = 'True
  TypeIsMaybe _         = 'False

type family MaybeTypeIsMaybe (k1 :: Maybe *) :: Maybe Bool where
  MaybeTypeIsMaybe 'Nothing  = 'Nothing
  MaybeTypeIsMaybe ('Just x) = 'Just (TypeIsMaybe x)


type family MaybeList (k1 :: Maybe a) :: Maybe [a] where
  MaybeList ('Just x) = 'Just '[x]
  MaybeList _ = 'Nothing


type family LiftFstMaybe (k1 :: (Maybe a, b)) :: Maybe (a, b) where
  LiftFstMaybe '( 'Just a, b ) = 'Just '(a, b)
  LiftFstMaybe _ = 'Nothing

type family LiftSndMaybe (k1 :: (a, Maybe b)) :: Maybe (a, b) where
  LiftSndMaybe '( a, 'Just b ) = 'Just '(a, b)
  LiftSndMaybe _ = 'Nothing


type family Not (k1 :: Bool) :: Bool where
  Not 'True  = 'False
  Not 'False = 'True

type family MaybeNot (k1 :: Maybe Bool) :: Maybe Bool where
  MaybeNot 'Nothing  = 'Nothing
  MaybeNot ('Just x) = 'Just (Not x)


type family Uncons (k1 :: [a]) :: Maybe (a, [a]) where
  Uncons '[] = 'Nothing
  Uncons (x ': xs) = 'Just '(x, xs)

type family MaybeCons (k1 :: Maybe a) (k2 :: [a]) :: Maybe [a] where
  MaybeCons ('Just x) xs = 'Just (x ': xs)
  MaybeCons _ _ = 'Nothing

type family MaybeListCons (k1 :: a) (k2 :: Maybe [a]) :: Maybe [a] where
  MaybeListCons x ('Just xs) = 'Just (x ': xs)
  MaybeListCons _ _ = 'Nothing

type family MaybeMaybeCons (k1 :: Maybe a) (k2 :: Maybe [a]) :: Maybe [a] where
  MaybeMaybeCons ('Just x) ('Just xs) = 'Just (x ': xs)
  MaybeMaybeCons _ _ = 'Nothing

type family Concat (k1 :: [a]) (k2 :: [a]) :: [a] where
  Concat '[] xs = xs
  Concat (x ': xs) ys = x ': Concat xs ys

type family MaybeMaybeConcat (k1 :: Maybe [a])
                             (k2 :: Maybe [a])
                                 :: Maybe [a] where
  MaybeMaybeConcat 'Nothing _ = 'Nothing
  MaybeMaybeConcat _ 'Nothing = 'Nothing
  MaybeMaybeConcat ('Just '[]) ('Just ys) = 'Just ys
  MaybeMaybeConcat ('Just (x ': xs)) ('Just ys) = 'Just (x ': Concat xs ys)


-- | Helps to construct property definition reference in a type-safe way.
--
-- Result:
--
--   1. @Symbol@ is a field name
--   2. @Bool@ indicates a field is required
--   3. @*@ is a field type
type family TypeSafeSchemaProperty
            (k1 :: * -> *)
            (k2 :: Either Symbol (Symbol, Symbol))
            :: Maybe (Symbol, Bool, *)
            where
  TypeSafeSchemaProperty t ('Left fieldName) =
    MaybeTriplet
      (FieldName t fieldName)
      (MaybeNot (MaybeTypeIsMaybe (FieldType t fieldName)))
      (FieldType t fieldName)
  TypeSafeSchemaProperty t ('Right '(constructorName, fieldName)) =
    MaybeTriplet
      (ConstructorFieldName t constructorName fieldName)
      (MaybeNot (MaybeTypeIsMaybe
                (ConstructorFieldType t constructorName fieldName)))
      (ConstructorFieldType t constructorName fieldName)

typeSafeSchemaProperty
  :: forall t typeRep field propName fieldName isFieldRequired fieldType
  .  ( IsString propName
     , KnownSymbol fieldName
     , KnownBool isFieldRequired
     , ToSchema fieldType
     , typeRep ~ Rep t
     , TypeSafeSchemaProperty typeRep field
         ~ 'Just '(fieldName, isFieldRequired, fieldType)
     )
  => Proxy '(t, (field :: Either Symbol (Symbol, Symbol)))
  -- ^ Second value of type-level tuple is either just field name or constructor
  -- name and field name in that order to constrain specified constructor.
  -> Declare (Definitions Schema) (propName, Bool, Referenced Schema)

typeSafeSchemaProperty Proxy =
  typeSafeSchemaProperty' (Proxy :: Proxy
    '(typeRep, (field :: Either Symbol (Symbol, Symbol))))

-- | Alternative version of "typeSafeSchemaProperty" which presumes that
-- provided type is already a @(Rep ofSomething)@.
--
-- Could be useful in cases when @Rep ofType@ is modified.
typeSafeSchemaProperty'
  :: forall typeRep field propName fieldName isFieldRequired fieldType
  .  ( IsString propName
     , KnownSymbol fieldName
     , KnownBool isFieldRequired
     , ToSchema fieldType
     , TypeSafeSchemaProperty typeRep field
         ~ 'Just '(fieldName, isFieldRequired, fieldType)
     )
  => Proxy '(typeRep, (field :: Either Symbol (Symbol, Symbol)))
  -- ^ Second value of type-level tuple is either just field name or constructor
  -- name and field name in that order to constrain specified constructor.
  -> Declare (Definitions Schema) (propName, Bool, Referenced Schema)

typeSafeSchemaProperty' Proxy =
  declareSchemaRef (Proxy :: Proxy fieldType) <&> \schemaRef ->
    ( fromString $ symbolVal (Proxy :: Proxy fieldName)
    , boolVal (Proxy :: Proxy isFieldRequired)
    , schemaRef
    )


type family TypeSafeSchemaProperties
            (k1 :: * -> *)
            (k2 :: Symbol)
            :: Maybe [(Symbol, Bool, *)]
            where
  TypeSafeSchemaProperties (D1 a (C1 ('MetaCons c b 'True) x)) c =
    ConstructorProperties (D1 a (C1 ('MetaCons c b 'True) x)) c x ('Just '[])
  TypeSafeSchemaProperties (D1 a (C1 ('MetaCons c b 'True) x :+: _)) c =
    ConstructorProperties (D1 a (C1 ('MetaCons c b 'True) x)) c x ('Just '[])
  TypeSafeSchemaProperties (D1 a (C1 ('MetaCons _ _ _) _ :+: xs)) c =
    TypeSafeSchemaProperties (D1 a xs) c
  TypeSafeSchemaProperties (D1 a (xs :+: ys)) c =
    MaybeAlternative
      (TypeSafeSchemaProperties (D1 a xs) c)
      (TypeSafeSchemaProperties (D1 a ys) c)
  TypeSafeSchemaProperties (D1 _ _) _ = 'Nothing

type family ConstructorProperties
            (k1 :: * -> *)
            (k2 :: Symbol)
            (k3 :: * -> *)
            (k4 :: Maybe [(Symbol, Bool, *)])
            :: Maybe [(Symbol, Bool, *)]
            where

  ConstructorProperties t c
                        (S1 ('MetaSel ('Just f) _ _ _) (Rec0 _))
                        ('Just acc) =
    MaybeMaybeCons (TypeSafeSchemaProperty t ('Right '(c, f))) ('Just acc)

  ConstructorProperties t c
                        (S1 ('MetaSel ('Just f) _ _ _) (Rec0 _) :*: xs)
                        ('Just acc) =
    MaybeMaybeCons
      (TypeSafeSchemaProperty t ('Right '(c, f)))
      (ConstructorProperties t c xs ('Just acc))

  ConstructorProperties t c (xs :*: ys) ('Just acc) =
    MaybeMaybeConcat
      (ConstructorProperties t c xs ('Just '[]))
      (ConstructorProperties t c ys ('Just acc))

  ConstructorProperties (D1 a b) _ (S1 _ _) _ = 'Nothing

typeSafeSchemaProperties
  :: forall t typeRep constructorName propName list
  .  ( typeRep ~ Rep t
     , TypeSafeSchemaProperties typeRep constructorName ~ 'Just list
     , TypeSafeSchemaPropertiesI list
     , IsString propName
     )
  => Proxy '(t, constructorName)
  -> Declare (Definitions Schema) [(propName, Bool, Referenced Schema)]

typeSafeSchemaProperties Proxy =
  typeSafeSchemaPropertiesI (Proxy :: Proxy list)

-- | Alternative version of "typeSafeSchemaProperties" which presumes that
-- provided type is already a @(Rep ofSomething)@.
--
-- Could be useful in cases when @Rep ofType@ is modified.
typeSafeSchemaProperties'
  :: forall typeRep constructorName propName list
  .  ( TypeSafeSchemaProperties typeRep constructorName ~ 'Just list
     , TypeSafeSchemaPropertiesI list
     , IsString propName
     )
  => Proxy '(typeRep, constructorName)
  -> Declare (Definitions Schema) [(propName, Bool, Referenced Schema)]

typeSafeSchemaProperties' Proxy =
  typeSafeSchemaPropertiesI (Proxy :: Proxy list)

typeSafeSchemaSeparatedProperties
  :: forall t typeRep constructorName propName requiredPropName list
  .  ( typeRep ~ Rep t
     , TypeSafeSchemaProperties typeRep constructorName ~ 'Just list
     , TypeSafeSchemaPropertiesI list
     , IsString propName
     , IsString requiredPropName
     )
  => Proxy '(t, constructorName)
  -> Declare (Definitions Schema)
             ([(propName, Referenced Schema)], [requiredPropName])

typeSafeSchemaSeparatedProperties Proxy =
  typeSafeSchemaSeparatedPropertiesI (Proxy :: Proxy list)

-- | Alternative version of "typeSafeSchemaSeparatedProperties" which presumes
-- that provided type is already a @(Rep ofSomething)@.
--
-- Could be useful in cases when @Rep ofType@ is modified.
typeSafeSchemaSeparatedProperties'
  :: forall typeRep constructorName propName requiredPropName list
  .  ( TypeSafeSchemaProperties typeRep constructorName ~ 'Just list
     , TypeSafeSchemaPropertiesI list
     , IsString propName
     , IsString requiredPropName
     )
  => Proxy '(typeRep, constructorName)
  -> Declare (Definitions Schema)
             ([(propName, Referenced Schema)], [requiredPropName])

typeSafeSchemaSeparatedProperties' Proxy =
  typeSafeSchemaSeparatedPropertiesI (Proxy :: Proxy list)


class TypeSafeSchemaPropertiesI (a :: [(Symbol, Bool, *)]) where

  typeSafeSchemaPropertiesI
    :: IsString propName
    => Proxy a
    -> Declare (Definitions Schema) [(propName, Bool, Referenced Schema)]

  typeSafeSchemaSeparatedPropertiesI
    :: (IsString propName, IsString requiredPropName)
    => Proxy a
    -> Declare (Definitions Schema)
               ([(propName, Referenced Schema)], [requiredPropName])

instance TypeSafeSchemaPropertiesI '[] where
  typeSafeSchemaPropertiesI Proxy = pure []
  typeSafeSchemaSeparatedPropertiesI Proxy = pure ([], [])

instance ( KnownSymbol propName
         , KnownBool isRequired
         , ToSchema t
         , TypeSafeSchemaPropertiesI xs
         ) => TypeSafeSchemaPropertiesI ('(propName, isRequired, t) ': xs)
         where

  typeSafeSchemaPropertiesI Proxy = (:)
    <$> (declareSchemaRef (Proxy :: Proxy t) <&> \schemaRef ->
          ( fromString $ symbolVal (Proxy :: Proxy propName)
          , boolVal (Proxy :: Proxy isRequired)
          , schemaRef
          ))
    <*> typeSafeSchemaPropertiesI (Proxy :: Proxy xs)

  typeSafeSchemaSeparatedPropertiesI Proxy = do
    schemaRef <- declareSchemaRef (Proxy :: Proxy t)
    typeSafeSchemaSeparatedPropertiesI (Proxy :: Proxy xs) <&> \(a, b) ->
      ( (fromString (symbolVal (Proxy :: Proxy propName)), schemaRef) : a
      , if boolVal (Proxy :: Proxy isRequired)
           then fromString (symbolVal (Proxy :: Proxy propName)) : b
           else b
      )


type family TypeSafeSchemaConstructorsAsProperties
            (k1 :: * -> *)
            :: Maybe [( Symbol, [(Symbol, Bool, *)] )]
            where

  TypeSafeSchemaConstructorsAsProperties (D1 a (C1 ('MetaCons c b 'True) x)) =
    MaybeList
      (LiftSndMaybe
        '(c, TypeSafeSchemaProperties (D1 a (C1 ('MetaCons c b 'True) x)) c))

  TypeSafeSchemaConstructorsAsProperties
    (D1 a (C1 ('MetaCons c b 'True) x :+: xs)) =
      MaybeMaybeCons
        (LiftSndMaybe
          '(c, (TypeSafeSchemaProperties
               (D1 a (C1 ('MetaCons c b 'True) x)) c)))
        (TypeSafeSchemaConstructorsAsProperties (D1 a xs))

  TypeSafeSchemaConstructorsAsProperties (D1 a (xs :+: ys)) =
    MaybeMaybeConcat
      (TypeSafeSchemaConstructorsAsProperties (D1 a xs))
      (TypeSafeSchemaConstructorsAsProperties (D1 a ys))

  TypeSafeSchemaConstructorsAsProperties (D1 _ _) = 'Nothing

typeSafeSchemaConstructorsAsProperties
  :: forall t typeRep constructorName list
  .  ( typeRep ~ Rep t
     , TypeSafeSchemaConstructorsAsProperties typeRep ~ 'Just list
     , TypeSafeSchemaConstructorsAsPropertiesI list
     , IsString constructorName
     )
  => Proxy t
  -> Declare (Definitions Schema) [(constructorName, Referenced Schema)]

typeSafeSchemaConstructorsAsProperties Proxy =
  typeSafeSchemaConstructorsAsProperties' (Proxy :: Proxy typeRep)

-- | Alternative version of "typeSafeSchemaConstructorsAsProperties" which
-- presumes that provided type is already a @(Rep ofSomething)@.
--
-- Could be useful in cases when @Rep ofType@ is modified.
typeSafeSchemaConstructorsAsProperties'
  :: forall typeRep constructorName list
  .  ( TypeSafeSchemaConstructorsAsProperties typeRep ~ 'Just list
     , TypeSafeSchemaConstructorsAsPropertiesI list
     , IsString constructorName
     )
  => Proxy typeRep
  -> Declare (Definitions Schema) [(constructorName, Referenced Schema)]

typeSafeSchemaConstructorsAsProperties' p@Proxy =
  typeSafeSchemaMapConstructorsAsProperties' p $ const id

typeSafeSchemaMapConstructorsAsProperties
  :: forall t typeRep constructorName list
  .  ( typeRep ~ Rep t
     , TypeSafeSchemaConstructorsAsProperties typeRep ~ 'Just list
     , TypeSafeSchemaConstructorsAsPropertiesI list
     , IsString constructorName
     )
  => Proxy t
  -> (constructorName -> Schema -> Schema)
  -> Declare (Definitions Schema) [(constructorName, Referenced Schema)]

typeSafeSchemaMapConstructorsAsProperties Proxy =
  typeSafeSchemaMapConstructorsAsProperties' (Proxy :: Proxy typeRep)

-- | Alternative version of "typeSafeSchemaMapConstructorsAsProperties" which
-- presumes that provided type is already a @(Rep ofSomething)@.
--
-- Could be useful in cases when @Rep ofType@ is modified.
typeSafeSchemaMapConstructorsAsProperties'
  :: forall typeRep constructorName list
  .  ( TypeSafeSchemaConstructorsAsProperties typeRep ~ 'Just list
     , TypeSafeSchemaConstructorsAsPropertiesI list
     , IsString constructorName
     )
  => Proxy typeRep
  -> (constructorName -> Schema -> Schema)
  -> Declare (Definitions Schema) [(constructorName, Referenced Schema)]

typeSafeSchemaMapConstructorsAsProperties' Proxy schemaMapFn =
  typeSafeSchemaConstructorsAsSeparatedPropertiesI (Proxy :: Proxy list)
    <&> fmap f
  where
    f (constructor, (props, requiredProps)) =
      ( constructor
      , Inline $ schemaMapFn constructor $ mempty
          { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
          , _schemaProperties  = fromList props
          , _schemaRequired    = fromList requiredProps
          }
      )

class TypeSafeSchemaConstructorsAsPropertiesI
      (a :: [( Symbol, [(Symbol, Bool, *)] )]) where

  typeSafeSchemaConstructorsAsPropertiesI
    :: (IsString constructorName, IsString propName)
    => Proxy a
    -> Declare (Definitions Schema)
               [(constructorName, [(propName, Bool, Referenced Schema)])]

  typeSafeSchemaConstructorsAsSeparatedPropertiesI
    :: (IsString constructorName, IsString propName, IsString requiredPropName)
    => Proxy a
    -> Declare (Definitions Schema)
               [ ( constructorName
                 , ([(propName, Referenced Schema)], [requiredPropName])
                 ) ]

instance TypeSafeSchemaConstructorsAsPropertiesI '[] where
  typeSafeSchemaConstructorsAsPropertiesI Proxy = pure []
  typeSafeSchemaConstructorsAsSeparatedPropertiesI Proxy = pure []

instance ( KnownSymbol constructorName
         , TypeSafeSchemaPropertiesI props
         , TypeSafeSchemaConstructorsAsPropertiesI xs
         ) => TypeSafeSchemaConstructorsAsPropertiesI
              ('(constructorName, props) ': xs)
         where

  typeSafeSchemaConstructorsAsPropertiesI Proxy = (:)
    <$> ((c,) <$> typeSafeSchemaPropertiesI (Proxy :: Proxy props))
    <*> typeSafeSchemaConstructorsAsPropertiesI (Proxy :: Proxy xs)
    where c = fromString $ symbolVal (Proxy :: Proxy constructorName)

  typeSafeSchemaConstructorsAsSeparatedPropertiesI Proxy = (:)
    <$> ((c,) <$> typeSafeSchemaSeparatedPropertiesI (Proxy :: Proxy props))
    <*> typeSafeSchemaConstructorsAsSeparatedPropertiesI (Proxy :: Proxy xs)
    where c = fromString $ symbolVal (Proxy :: Proxy constructorName)


-- | Prototype "Schema" for branching constructors
constructorsBranchingSchemaProto :: Schema
constructorsBranchingSchemaProto = mempty
  { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
  , _schemaMinProperties = Just 1
  , _schemaMaxProperties = Just 1
  }
