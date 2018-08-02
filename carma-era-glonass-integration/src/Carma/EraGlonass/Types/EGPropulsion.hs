{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Carma.EraGlonass.Types.EGPropulsion
     ( EGPropulsion (..)
     ) where

import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Swagger
import           Text.InterpolatedString.QM


data EGPropulsion
   = Hydrogen
   | Electricity
   | LPG
   | LNG
   | Diesel
   | Gasoline
     deriving (Show, Eq)

instance FromJSON EGPropulsion where
  parseJSON (String "HYDROGEN")    = pure Hydrogen
  parseJSON (String "ELECTRICITY") = pure Electricity
  parseJSON (String "LPG")         = pure LPG
  parseJSON (String "LNG")         = pure LNG
  parseJSON (String "DIESEL")      = pure Diesel
  parseJSON (String "GASOLINE")    = pure Gasoline
  parseJSON x                      = typeMismatch "EGPropulsion" x

instance ToSchema EGPropulsion where
  declareNamedSchema _ = pure
    $ NamedSchema (Just "EGPropulsion") mempty
    { _schemaParamSchema = mempty
        { _paramSchemaType    = SwaggerString
        , _paramSchemaFormat  = Just "propulsion"
        , _paramSchemaPattern = Just
            [qn| ^(HYDROGEN
                  |ELECTRICITY
                  |LPG
                  |LNG
                  |DIESEL
                  |GASOLINE
                  )$ |]
        }
    }
