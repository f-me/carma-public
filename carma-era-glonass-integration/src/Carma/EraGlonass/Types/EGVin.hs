{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, TypeFamilies, InstanceSigs #-}

module Carma.EraGlonass.Types.EGVin
     ( EGVin (..)
     ) where

import           GHC.Generics

import           Data.Proxy
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.String (IsString (fromString))
import           Text.InterpolatedString.QM
import           Data.Aeson
import           Data.Aeson.Types (Parser, typeMismatch)
import           Data.Swagger hiding (delete)
import           Data.Swagger.Declare (Declare)
import           Data.Attoparsec.ByteString.Char8 hiding (Parser)
import           Data.ByteString.Char8 (ByteString)
import           Data.List (delete)
import           Data.Char (toUpper)
import           Data.Monoid ((<>))

import           Carma.Utils.Operators
import           Carma.Utils.TypeSafe.Generic.DataType


-- You could read about VIN here:
-- https://en.wikipedia.org/wiki/Vehicle_identification_number
-- So it is alphanumeric of 17 characters excluding O/o, I/i and Q/q.
newtype EGVin
      = EGVin { fromEGVin :: ByteString }
        deriving (Show, Eq, Generic)

instance IsString EGVin where
  fromString = EGVin . fromString

instance FromJSON EGVin where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  parseJSON :: forall t. t ~ EGVin => Value -> Parser t

  parseJSON j@(String x) =
    case parseOnly parser (encodeUtf8 x) of
         Left  _ -> typeMismatch (typeName (Proxy :: Proxy t)) j
         Right y -> pure y

    where -- It isn't exhaustive constraint but we don't have to be that smart
          -- here. Also keeping result case-sensitive (but not constraint) to
          -- keep value as original as possible.
          parser = EGVin . fromString
            <$> count 17 (satisfy isVinChar)
            <*  endOfInput

            where -- Also correct: ['A'..'H']<>['J'..'N']<>('P':['R'..'Z'])
                  vinChars =
                    foldr delete ['A'..'Z'] ("IOQ" :: String) <> ['0'..'9']

                  isVinChar = toUpper ? (`elem` vinChars)

  parseJSON x = typeMismatch (typeName (Proxy :: Proxy t)) x

instance ToJSON EGVin where
  toJSON (EGVin x) = toJSON $ decodeUtf8 x

instance ToSchema EGVin where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  declareNamedSchema
    :: forall proxy t. t ~ EGVin
    => proxy t
    -> Declare (Definitions Schema) NamedSchema

  declareNamedSchema _ = pure
    $ NamedSchema (Just $ typeName (Proxy :: Proxy t)) mempty
    { _schemaParamSchema = mempty
        { _paramSchemaType    = SwaggerString
        , _paramSchemaFormat  = Just "VIN"
        , _paramSchemaPattern = Just [qn| ^[A-HJ-NPR-Z0-9]{17}$ |]
        }
    }
