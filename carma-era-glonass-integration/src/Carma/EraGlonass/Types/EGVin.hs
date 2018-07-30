{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Carma.EraGlonass.Types.EGVin
     ( EGVin (EGVin)
     ) where

import           Data.Text.Encoding (encodeUtf8)
import           Data.String (IsString (fromString))
import           Text.InterpolatedString.QM
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Swagger hiding (delete)
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8 (ByteString)
import           Data.List (delete)
import           Data.Char (toUpper)
import           Data.Monoid ((<>))

import           Carma.Utils.Operators


-- You could read about VIN here:
-- https://en.wikipedia.org/wiki/Vehicle_identification_number
-- So it is alphanumeric of 17 characters excluding O/o, I/i and Q/q.
newtype EGVin = EGVin ByteString deriving (Show, Eq)

instance IsString EGVin where
  fromString = EGVin . fromString

instance FromJSON EGVin where
  parseJSON j@(String x) =
    case parseOnly parser (encodeUtf8 x) of
         Left  _ -> typeMismatch "EGVin" j
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


  parseJSON x = typeMismatch "EGPhoneNumber" x

instance ToSchema EGVin where
  declareNamedSchema _ = pure
    $ NamedSchema (Just "EGVin") mempty
    { _schemaParamSchema = mempty
        { _paramSchemaType    = SwaggerString
        , _paramSchemaFormat  = Just "VIN"
        , _paramSchemaPattern = Just [qn| ^[A-HJ-NPR-Z0-9]{17}$ |]
        }
    }
