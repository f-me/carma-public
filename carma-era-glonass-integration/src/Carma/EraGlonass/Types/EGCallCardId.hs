{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Carma.EraGlonass.Types.EGCallCardId
     ( EGCallCardId (EGCallCardId)
     ) where

import           Data.Monoid ((<>))
import           Data.Function ((&))
import           Data.Text
import           Data.Text.Encoding (encodeUtf8)
import           Text.InterpolatedString.QM
import           Data.String (fromString)
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Swagger
import           Data.Attoparsec.ByteString.Char8


newtype EGCallCardId = EGCallCardId Text deriving (Eq, Show)

instance FromJSON EGCallCardId where
  parseJSON v@(String x)
    = parseOnly parser (encodeUtf8 x)
    & \case Left  _ -> typeMismatch "EGCallCardId" v
            Right y -> pure y

    where parser = EGCallCardId . fromString
            <$> many1 (satisfy (`elem` chars))
            <*  endOfInput
            where chars = ['a'..'f'] <> ['A'..'F'] <> ['0'..'9']

  parseJSON invalid = typeMismatch "EGCallCardId" invalid

instance ToSchema EGCallCardId where
  declareNamedSchema _ = pure $ NamedSchema (Just "EGCallCardId") mempty
    { _schemaParamSchema = mempty
        { _paramSchemaType    = SwaggerString
        , _paramSchemaFormat  = Just "hex-hash"
        , _paramSchemaPattern = Just [qn| ^[0-9a-zA-Z]+$ |]
        }
    }
