{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Carma.EraGlonass.Types.EGPhoneNumber
     ( EGPhoneNumber (..)
     ) where

import           Data.Maybe
import           Data.Text hiding (count)
import           Data.String (IsString (fromString))
import           Data.Text.Encoding (encodeUtf8)
import           Text.InterpolatedString.QM
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Swagger
import           Data.Attoparsec.ByteString.Char8

import           Control.Applicative ((<|>))


-- A string from 1 to 18 chars of numbers which could be prefixed with '+'
newtype EGPhoneNumber
      = EGPhoneNumber { fromEGPhoneNumber :: Text }
        deriving (Show, Eq)

instance IsString EGPhoneNumber where
  fromString = EGPhoneNumber . fromString

instance FromJSON EGPhoneNumber where
  parseJSON j@(String x) =
    case parseOnly parser (encodeUtf8 x) of
         Left  _ -> typeMismatch "EGPhoneNumber" j
         Right y -> pure y

    where -- Optional plus prefix (1 char)
          --   + 1 required digit
          --   + 16 optional digits
          --   = 18 chars
          -- Maximum amount of digits is 17 where 18th char is just optional
          -- plus prefix.
          parser = f
            <$> optionalPlus
            <*> digit
            <*> count 16 optionalDigit
            <*  endOfInput
            where optionalPlus  = (Just <$> char '+') <|> pure Nothing
                  optionalDigit = (Just <$> digit)    <|> pure Nothing

                  f plus a b = EGPhoneNumber
                             $ fromString
                             $ maybe id (:) plus
                             $ a : catMaybes b

  parseJSON x = typeMismatch "EGPhoneNumber" x

instance ToJSON EGPhoneNumber where
  toJSON (EGPhoneNumber x) = toJSON x

instance ToSchema EGPhoneNumber where
  declareNamedSchema _ = pure
    $ NamedSchema (Just "EGPhoneNumber") mempty
    { _schemaParamSchema = mempty
        { _paramSchemaType    = SwaggerString
        , _paramSchemaFormat  = Just "phone"
        , _paramSchemaPattern = Just [qn| ^\+?[0-9]{1,17}$ |]
        }
    }
