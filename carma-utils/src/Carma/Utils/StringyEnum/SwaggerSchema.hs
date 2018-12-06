{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

-- | Default implementations for "ToSchema" based on "StringyEnum" instance.
module Carma.Utils.StringyEnum.SwaggerSchema
     ( stringyEnumNamedSchema
     , stringyEnumMappedNamedSchema
     ) where

import           GHC.Generics

import           Data.Proxy
import           Data.Aeson
import           Data.Swagger
import           Data.Swagger.Declare
import           Data.Swagger.Internal.Schema

import           Carma.Utils.StringyEnum


-- | Default implementation of "ToSchema" instance based on "StringyEnum"
-- instance.
stringyEnumNamedSchema
  :: forall a typeRep proxy
  .  ( typeRep ~ Rep a
     , GToSchema typeRep
     , ToJSON a
     , StringyEnum a
     , Enum a
     , Bounded a
     )
  => proxy a
  -> Declare (Definitions Schema) NamedSchema

stringyEnumNamedSchema = flip stringyEnumMappedNamedSchema pure


-- | An alternative version of "stringyEnumNamedSchema" with ability to modify
-- produced "NamedSchema".
stringyEnumMappedNamedSchema
  :: forall a typeRep proxy
  .  ( typeRep ~ Rep a
     , GToSchema typeRep
     , ToJSON a
     , StringyEnum a
     , Enum a
     , Bounded a
     )
  => proxy a
  -> (NamedSchema -> Declare (Definitions Schema) NamedSchema)
  -> Declare (Definitions Schema) NamedSchema

stringyEnumMappedNamedSchema _ mapFn = do
  NamedSchema name' schema' <-
    gdeclareNamedSchema defaultSchemaOptions (Proxy :: Proxy typeRep) mempty

  mapFn $ NamedSchema name' schema'
    { _schemaParamSchema = (_schemaParamSchema schema')
        { _paramSchemaType = SwaggerString
        , _paramSchemaEnum =
            Just $ String . toStringy <$> [minBound .. maxBound :: a]
        }
    }
