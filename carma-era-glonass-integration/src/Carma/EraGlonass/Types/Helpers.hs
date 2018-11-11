{-# LANGUAGE DataKinds, TypeFamilies, FlexibleContexts #-}

module Carma.EraGlonass.Types.Helpers
     ( StringyEnum (..)
     , ReplaceFieldKey
     , stringyEnumNamedSchema
     ) where

import           GHC.Generics
import           GHC.TypeLits

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
