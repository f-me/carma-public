{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Carma.EraGlonass.Types.EGCallerFullName
     ( EGCallerFullName (EGCallerFullName)
     ) where

import           Data.Maybe
import           Data.Function ((&))
import           Data.Text (Text)
import           Text.InterpolatedString.QM
import           Data.String (fromString)
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Swagger
import           Data.Attoparsec.Text

import           Control.Applicative ((<|>))


newtype EGCallerFullName = EGCallerFullName Text deriving (Eq, Show)

instance FromJSON EGCallerFullName where
  parseJSON v@(String x)
    = parseOnly parser x
    & \case Left  _ -> typeMismatch "EGCallerFullName" v
            Right y -> pure y

    where parser = EGCallerFullName . fromString . catMaybes
            <$> count 50 ((Just <$> anyChar) <|> pure Nothing)
            <*  endOfInput

  parseJSON invalid = typeMismatch "EGCallerFullName" invalid

instance ToSchema EGCallerFullName where
  declareNamedSchema _ = pure $ NamedSchema (Just "EGCallerFullName") mempty
    { _schemaParamSchema = mempty
        { _paramSchemaType    = SwaggerString
        , _paramSchemaPattern = Just [qn| ^.{0,50}$ |]
        }
    }
