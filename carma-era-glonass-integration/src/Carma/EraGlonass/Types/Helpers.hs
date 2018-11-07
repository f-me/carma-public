{-# LANGUAGE FlexibleContexts, QuasiQuotes, LambdaCase #-}

module Carma.EraGlonass.Types.Helpers
     ( stringyEnumNamedSchema
     ) where

import           GHC.Generics (Rep)

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
         _ -> error [qms| "stringyEnumNamedSchema":
                          Every constructor must be resolved to
                          "String" by using "toJSON" |]

      where enum' :: (Enum a, Bounded a) => proxy a -> [a]
            enum' _ = [minBound..maxBound]
