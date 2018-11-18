{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE PolyKinds, UndecidableInstances #-}

module Carma.EraGlonass.Types.Helpers
     ( StringyEnum (..)
     , ReplaceFieldKey
     , stringyEnumNamedSchema

     , TypeName
     , typeName
     , ConstructorName
     , constructorName
     , FieldName
     , fieldName
     , ConstructorFieldName
     , constructorFieldName
     , FieldType
     , fieldType
     , ConstructorFieldType
     , constructorFieldType

     , addConstructorTag
     , proxyPair
     , proxyTriplet
     , proxyPair2Triplet
     ) where

import           GHC.Generics
import           GHC.TypeLits

import           Data.Proxy
import qualified Data.HashMap.Lazy as HM
import           Data.String (IsString (fromString))
import           Data.Text (Text)
import           Data.Aeson
import           Data.Swagger
import           Data.Swagger.Declare
import           Data.Swagger.Internal.Schema


stringyEnumNamedSchema
  :: (GToSchema (Rep a), ToJSON a, StringyEnum a, Enum a, Bounded a)
  => proxy a
  -> Declare (Definitions Schema) NamedSchema

stringyEnumNamedSchema p = do
  NamedSchema name' schema' <-
    gdeclareNamedSchema defaultSchemaOptions (repProxy p) mempty

  pure $ NamedSchema name' schema'
    { _schemaParamSchema = (_schemaParamSchema schema')
        { _paramSchemaType = SwaggerString
        , _paramSchemaEnum = Just $ wholeEnum p
        }
    }

  where
    repProxy :: proxy p -> Proxy (Rep p)
    repProxy _ = Proxy

    wholeEnum :: (StringyEnum a, Enum a, Bounded a) => proxy a -> [Value]
    wholeEnum = fmap (String . toStringy) . enum'
      where enum' :: (StringyEnum a, Enum a, Bounded a) => proxy a -> [a]
            enum' _ = [minBound..maxBound]


class StringyEnum a where
  toStringy :: a -> Text


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
typeName :: (KnownSymbol (TypeName (Rep a)), IsString str) => Proxy a -> str
typeName = fromString . symbolVal . f where
  f :: Proxy p -> Proxy (TypeName (Rep p))
  f Proxy = Proxy


-- | Helps to use constructor name of a type with protection of its correctness.
type family ConstructorName (k1 :: * -> *) (k2 :: Symbol) :: Maybe Symbol where
  ConstructorName (D1 _ (C1 ('MetaCons x _ _) _)) x = 'Just x
  ConstructorName (D1 _ (C1 ('MetaCons x _ _) _ :+: _)) x = 'Just x
  ConstructorName (D1 a (C1 _ _ :+: xs)) x = ConstructorName (D1 a xs) x
  ConstructorName (D1 _ _) _ = 'Nothing

-- | Helps to use constructor name of a proxied type with protection of its
-- correctness.
constructorName
  :: ( ConstructorName (Rep t) constructor ~ 'Just constructor
     , KnownSymbol constructor
     , IsString str
     )
  => Proxy '(t, constructor)
  -> str

constructorName = fromString . symbolVal . f where
  f :: KnownSymbol b => Proxy '(a, b) -> Proxy b
  f Proxy = Proxy


-- | Helps to use a field name of a record type with protection of its
-- correctness.
type family FieldName (k1 :: * -> *) (k2 :: Symbol) :: Maybe Symbol where
  FieldName (D1 _ (C1 ('MetaCons _ _ _) x)) s = RecordFieldName x s
  FieldName (D1 a (C1 ('MetaCons _ _ _) x :+: xs)) s =
    MaybeAlternative (RecordFieldName x s) (FieldName (D1 a xs) s)
  FieldName (D1 _ _) _ = 'Nothing

-- | Helps to use field name of a proxied record type with protection of its
-- correctness.
fieldName
  :: ( FieldName (Rep t) field ~ 'Just field
     , KnownSymbol field
     , IsString str
     )
  => Proxy '(t, field)
  -> str

fieldName = fromString . symbolVal . f where
  f :: KnownSymbol b => Proxy '(a, b) -> Proxy b
  f Proxy = Proxy

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
  ConstructorFieldName (D1 _ _) _ _ = 'Nothing

-- | Helps to use a field name of a proxied record type with protection of its
-- correctness and that this field defined inside provided constructor.
constructorFieldName
  :: ( ConstructorFieldName (Rep t) constructor field ~ 'Just field
     , KnownSymbol field
     , IsString str
     )
  => Proxy '(t, constructor, field)
  -> str

constructorFieldName = fromString . symbolVal . f where
  f :: KnownSymbol c => Proxy '(a, b, c) -> Proxy c
  f Proxy = Proxy


-- | Helps to use a field type of a record type with protection of its
-- correctness (you couldn't obtain a type by name of a field which isn't
-- defined).
type family FieldType (k1 :: * -> *) (k2 :: Symbol) :: Maybe * where
  FieldType (D1 _ (C1 ('MetaCons _ _ 'True) x)) s = RecordFieldType x s
  FieldType (D1 a (C1 ('MetaCons _ _ 'True) x :+: xs)) s =
    MaybeAlternative (RecordFieldType x s) (FieldType (D1 a xs) s)
  FieldType (D1 _ _) _ = 'Nothing

-- | Helps to use a field type of a proxied record type with protection of its
-- correctness (you couldn't obtain a type by name of a field which isn't
-- defined).
fieldType
  :: (FieldType (Rep recordType) fieldName ~ 'Just fieldType)
  => Proxy '(recordType, fieldName)
  -> Proxy fieldType

fieldType Proxy = Proxy

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
  ConstructorFieldType (D1 _ _) _ _ = 'Nothing

-- | Helps to use a field type of a proxied record type with protection of its
-- correctness and that this field defined inside provided constructor
-- (you couldn't obtain a type by name of a field which isn't defined).
constructorFieldType
  :: ( ConstructorFieldType (Rep recordType) constructorName fieldName
     ~ 'Just fieldType
     )
  => Proxy '(recordType, constructorName, fieldName)
  -> Proxy fieldType

constructorFieldType Proxy = Proxy


-- | Sub-helper of @FieldName@ and @ConstructorName@ to iterate over fields
-- definitions inside a constructor.
type family RecordFieldName (k1 :: * -> *) (k2 :: Symbol) :: Maybe Symbol where
  RecordFieldName (S1 ('MetaSel ('Just x) _ _ _) _) x = 'Just x
  RecordFieldName (S1 ('MetaSel ('Just x) _ _ _) _ :*: _) x = 'Just x
  RecordFieldName (S1 ('MetaSel _ _ _ _) _ :*: xs) x = RecordFieldName xs x
  RecordFieldName (S1 _ _) _ = 'Nothing

-- | Sub-helper of @FieldType@ and @ConstructorFieldType@ to iterate over fields
-- definitions inside a constructor.
type family RecordFieldType (k1 :: * -> *) (k2 :: Symbol) :: Maybe * where
  RecordFieldType (S1 ('MetaSel ('Just x) _ _ _) (Rec0 t)) x = 'Just t
  RecordFieldType (S1 ('MetaSel ('Just x) _ _ _) (Rec0 t) :*: _) x = 'Just t
  RecordFieldType (S1 ('MetaSel _ _ _ _) _ :*: xs) x = RecordFieldType xs x
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
  :: ( ConstructorName (Rep t) constructor ~ 'Just constructor
     , KnownSymbol constructor
     )
  => Proxy '(t, constructor)
  -> Object
  -> Object

addConstructorTag p@Proxy = HM.insert "tag" $ String $ constructorName p


proxyPair :: Proxy a -> Proxy b -> Proxy '(a, b)
proxyPair Proxy Proxy = Proxy

proxyTriplet :: Proxy a -> Proxy b -> Proxy c -> Proxy '(a, b, c)
proxyTriplet Proxy Proxy Proxy = Proxy

proxyPair2Triplet :: Proxy '(a, b) -> Proxy c -> Proxy '(a, b, c)
proxyPair2Triplet Proxy Proxy = Proxy


type family MaybeAlternative (k1 :: Maybe k)
                             (k2 :: Maybe k)
                                 :: Maybe k where
  MaybeAlternative ('Just x) _        = 'Just x
  MaybeAlternative 'Nothing ('Just x) = 'Just x
  MaybeAlternative 'Nothing 'Nothing  = 'Nothing
