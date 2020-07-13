{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module Carma.Utils.TypeSafe.Generic.Swagger
     ( TypeSafeSchemaProperty
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
     ) where

import           GHC.Generics
import           GHC.TypeLits
import           GHC.Exts (IsList (..))

import           Data.Proxy
import           Data.String (IsString (fromString))
import           Data.Swagger
import           Data.Swagger.Declare (Declare)

import           Carma.Utils.Operators
import           Carma.Utils.TypeSafe.KnownBool
import           Carma.Utils.TypeSafe.TypeFamilies
import           Carma.Utils.TypeSafe.Generic.Record


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
          { _schemaParamSchema = mempty { _paramSchemaType = Just SwaggerObject }
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
