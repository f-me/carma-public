{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, FlexibleContexts #-}

module Carma.EraGlonass.Types.Helpers
     ( StringyEnum (..)
     , ReplaceFieldKey
     , stringyEnumNamedSchema

     , TypeName
     , typeName
     , ConstructorName
     , constructorName
     ) where

import           GHC.Generics
import           GHC.TypeLits

import           Data.String (IsString (fromString))
import           Data.Text (Text)
import           Data.Proxy
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
