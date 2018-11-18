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

     , addConstructorTag
     , proxyPair
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
typeName
  :: (Generic a, KnownSymbol (TypeName (Rep a)), IsString str)
  => Proxy a
  -> str

typeName = fromString . symbolVal . f where
  f :: Proxy p -> Proxy (TypeName (Rep p))
  f Proxy = Proxy


-- | Helps to use constructor name of a type with protection of its correctness.
type family ConstructorName (k1 :: * -> *) (k2 :: Symbol) :: Maybe Symbol where
  ConstructorName (D1 _ (C1 ('MetaCons x _ _) _ :+: _)) x = 'Just x
  ConstructorName (D1 a (C1 _ _ :+: xs)) x = ConstructorName (D1 a xs) x
  ConstructorName (D1 _ (C1 ('MetaCons x _ _) _)) x = 'Just x
  ConstructorName _ _ = 'Nothing

-- | Helps to use constructor name of a proxied type with protection of its
-- correctness.
constructorName
  :: ( Generic t
     , ConstructorName (Rep t) constructor ~ 'Just constructor
     , KnownSymbol constructor
     , IsString str
     )
  => Proxy '(t, constructor)
  -> str

constructorName = fromString . symbolVal . f where
  f :: KnownSymbol b => Proxy '(a, b) -> Proxy b
  f Proxy = Proxy


-- | Helps to use field name of a record type with protection of its
-- correctness.
type family FieldName (k1 :: * -> *) (k2 :: Symbol) :: Maybe Symbol where
  FieldName (D1 a (C1 ('MetaCons _ _ _) x :+: xs)) s =
    FieldNameAlt (RecordFieldName x s) (FieldName (D1 a xs) s)
  FieldName (D1 _ (C1 ('MetaCons _ _ _) x)) s = RecordFieldName x s
  FieldName _ _ = 'Nothing

-- | Sub-helper of @FieldName@
type family FieldNameAlt (k1 :: Maybe Symbol)
                         (k2 :: Maybe Symbol)
                             :: Maybe Symbol where
  FieldNameAlt ('Just x) _ = 'Just x
  FieldNameAlt _ ('Just x) = 'Just x
  FieldNameAlt _ _ = 'Nothing

-- | Sub-helper of @FieldName@
type family RecordFieldName (k1 :: * -> *) (k2 :: Symbol) :: Maybe Symbol where
  RecordFieldName (S1 ('MetaSel ('Just x) _ _ _) _ :*: _) x = 'Just x
  RecordFieldName (S1 ('MetaSel _ _ _ _) _ :*: xs) x = RecordFieldName xs x
  RecordFieldName (S1 ('MetaSel ('Just x) _ _ _) _) x = 'Just x
  RecordFieldName _ _ = 'Nothing

-- | Helps to use field name of a proxied record type with protection of its
-- correctness.
fieldName
  :: ( Generic t
     , FieldName (Rep t) field ~ 'Just field
     , KnownSymbol field
     , IsString str
     )
  => Proxy '(t, field)
  -> str

fieldName = fromString . symbolVal . f where
  f :: KnownSymbol b => Proxy '(a, b) -> Proxy b
  f Proxy = Proxy


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
  :: ( Generic t
     , ConstructorName (Rep t) constructor ~ 'Just constructor
     , KnownSymbol constructor
     )
  => Proxy '(t, constructor)
  -> Object
  -> Object

addConstructorTag p@Proxy = HM.insert "tag" $ String $ constructorName p


proxyPair :: Proxy a -> Proxy b -> Proxy '(a, b)
proxyPair Proxy Proxy = Proxy
