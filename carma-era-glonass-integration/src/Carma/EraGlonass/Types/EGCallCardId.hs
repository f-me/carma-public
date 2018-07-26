{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Carma.EraGlonass.Types.EGCallCardId
     ( EGCallCardId (EGCallCardId)
     ) where

import           Data.Function ((&))
import           Data.Text
import           Text.InterpolatedString.QM
import           Data.String (fromString)
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Swagger
import           Data.Attoparsec.Text


newtype EGCallCardId = EGCallCardId Text deriving (Eq, Show)

instance FromJSON EGCallCardId where
  parseJSON v@(String x)
    = parseOnly parser x
    & \case Left  _ -> typeMismatch "EGCallCardId" v
            Right y -> pure y

    where parser = EGCallCardId . fromString <$> many1 anyChar <* endOfInput

  parseJSON invalid = typeMismatch "EGCallCardId" invalid

instance ToSchema EGCallCardId where
  declareNamedSchema _ = pure $ NamedSchema (Just "EGCallCardId") mempty
    { _schemaParamSchema = mempty
        { _paramSchemaType    = SwaggerString
        , _paramSchemaPattern = Just [qn| .+ |]
        }
    }
