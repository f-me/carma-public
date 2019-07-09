{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs, TypeFamilies #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, QuasiQuotes #-}

module Carma.EraGlonass.Types.EGDateTime
     ( EGDateTime (..)
     ) where

import           GHC.Generics (Generic)

import           Data.Proxy
import           Data.Time.Clock (UTCTime)
import           Data.Text (Text)
import           Text.InterpolatedString.QM (qns)
import           Data.Aeson
import           Data.Aeson.Types (Parser, typeMismatch)
import           Data.Swagger

import           Carma.Utils.Operators
import           Carma.Utils.TypeSafe.Generic.DataType
import           Carma.EraGlonass.Types.Helpers.DateTime


newtype EGDateTime
      = EGDateTime UTCTime
        deriving (Generic, Eq, Show, Ord)

instance ToJSON EGDateTime where
  toJSON (EGDateTime x) = String $ showRFC3339DateTime x

instance FromJSON EGDateTime where
  parseJSON :: forall t. t ~ EGDateTime => Value -> Parser t
  parseJSON (String x) =
    either fail (pure . EGDateTime) $ parseRFC3339DateTime x
  parseJSON src = typeMismatch (typeName (Proxy :: Proxy t)) src

instance ToSchema EGDateTime where
  declareNamedSchema _ =
    declareNamedSchema (Proxy :: Proxy Text)
      <&> \case x@NamedSchema { _namedSchemaSchema = schema' } -> x
                  { _namedSchemaSchema
                      = schema'
                      { _schemaDescription = Just
                          [qns| RFC 3339 date-time.
                                Here are few examples of valid values:
                                "2019-02-28T15:00:49.561Z",
                                "2019-02-28T15:00:49.561+03:00",
                                "2019-02-28T15:00:49.561-02:00".
                                "Z" ending stands for UTC time.
                                See also https://www.ietf.org/rfc/rfc3339.txt |]
                      }
                  }
