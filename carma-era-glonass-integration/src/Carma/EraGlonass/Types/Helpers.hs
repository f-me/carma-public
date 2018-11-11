{-# LANGUAGE FlexibleContexts, QuasiQuotes, LambdaCase #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}

module Carma.EraGlonass.Types.Helpers
     ( ReplaceFieldKey
     , stringyEnumNamedSchema
     ) where

import           GHC.Generics
import           GHC.TypeLits

import           Text.InterpolatedString.QM
import           Data.Proxy
import           Data.Aeson
import           Data.Swagger
import           Data.Swagger.Declare
import           Data.Swagger.Internal.Schema

import           Carma.Utils.Operators


stringyEnumNamedSchema
  :: (GToSchema (Rep a), ToJSON a, Enum a, Bounded a)
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

    wholeEnum :: (ToJSON a, Enum a, Bounded a) => proxy a -> [Value]
    wholeEnum p' =
       enum' p' <&> toJSON <&> \case
         y@(String _) -> y
         y -> error [qms| "stringyEnumNamedSchema":
                          Every constructor must be resolved to
                          "String" by using "toJSON", recieved this: {y} |]

      where enum' :: (Enum a, Bounded a) => proxy a -> [a]
            enum' _ = [minBound..maxBound]


-- | Helps to rename a field of a record
type family ReplaceFieldKey (from :: Symbol) (to :: Symbol) field where
  ReplaceFieldKey from to (S1 ('MetaSel ('Just from) a b c) d) =
    S1 ('MetaSel ('Just to) a b c) d
