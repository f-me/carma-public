{-# LANGUAGE QuasiQuotes, LambdaCase #-}
{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, TypeFamilies, InstanceSigs #-}

module Carma.EraGlonass.Types.EGCallerFullName
     ( EGCallerFullName (..)
     ) where

import           GHC.Generics

import           Data.Proxy
import           Data.Maybe
import           Data.Function ((&))
import           Data.Text (Text)
import           Text.InterpolatedString.QM
import           Data.String (IsString (fromString))
import           Data.Aeson
import           Data.Aeson.Types (Parser, typeMismatch)
import           Data.Swagger
import           Data.Swagger.Declare (Declare)
import           Data.Attoparsec.Text hiding (Parser)

import           Control.Applicative ((<|>))

import           Carma.Utils.TypeSafe.Generic.DataType


newtype EGCallerFullName
      = EGCallerFullName { fromEGCallerFullName :: Text }
        deriving (Eq, Show, Generic)

instance IsString EGCallerFullName where
  fromString = EGCallerFullName . fromString

instance FromJSON EGCallerFullName where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  parseJSON :: forall t. t ~ EGCallerFullName => Value -> Parser t

  parseJSON v@(String x)
    = parseOnly parser x
    & \case Left  _ -> typeMismatch (typeName (Proxy :: Proxy t)) v
            Right y -> pure y

    where parser = EGCallerFullName . fromString . catMaybes
            <$> count 50 ((Just <$> anyChar) <|> pure Nothing)
            <*  endOfInput

  parseJSON invalid = typeMismatch (typeName (Proxy :: Proxy t)) invalid

instance ToJSON EGCallerFullName where
  toJSON (EGCallerFullName x) = toJSON x

instance ToSchema EGCallerFullName where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  declareNamedSchema
    :: forall proxy t. t ~ EGCallerFullName
    => proxy t
    -> Declare (Definitions Schema) NamedSchema

  declareNamedSchema _ = pure $ NamedSchema (Just typeName'') mempty
    { _schemaParamSchema = mempty
        { _paramSchemaType    = SwaggerString
        , _paramSchemaPattern = Just [qn| ^.{0,50}$ |]
        }
    }
    where typeName'' = typeName (Proxy :: Proxy t)
