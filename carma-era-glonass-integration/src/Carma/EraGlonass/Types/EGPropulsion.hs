{-# LANGUAGE OverloadedStrings, QuasiQuotes, LambdaCase #-}

module Carma.EraGlonass.Types.EGPropulsion
     ( EGPropulsion (..)
     , egPropulsionToEngineIdent
     , egPropulsionToEngineId
     ) where

import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Swagger
import           Text.InterpolatedString.QM
import           Data.Text (intercalate)

import           Data.Model.Types (Ident)
import qualified Carma.Model.Engine as Engine
import qualified Carma.Model.Engine.Persistent as EnginePersistent

import           Carma.Utils.Operators


data EGPropulsion
   = Hydrogen
   | Electricity
   | LPG
   | LNG
   | Diesel
   | Gasoline
     deriving (Show, Eq, Enum, Bounded)

instance ToJSON EGPropulsion where
  toJSON Hydrogen    = String "HYDROGEN"
  toJSON Electricity = String "ELECTRICITY"
  toJSON LPG         = String "LPG"
  toJSON LNG         = String "LNG"
  toJSON Diesel      = String "DIESEL"
  toJSON Gasoline    = String "GASOLINE"

instance FromJSON EGPropulsion where
  -- Producing list of all values to reduce human-factor mistakes,
  -- so it is handled automatically when we add a new value.
  parseJSON jsonValue = f [minBound..(maxBound :: EGPropulsion)]
    where f [] = typeMismatch "EGPropulsion" jsonValue
          f (x:xs) | toJSON x == jsonValue = pure x
                   | otherwise             = f xs

instance ToSchema EGPropulsion where
  declareNamedSchema _ = pure
    $ NamedSchema (Just "EGPropulsion") mempty
    { _schemaParamSchema = mempty
        { _paramSchemaType    = SwaggerString
        , _paramSchemaFormat  = Just "propulsion"
        , _paramSchemaEnum    = Just $ fst <$> wholeEnum
        , _paramSchemaPattern =
            Just [qm| ^({intercalate "|" $ snd <$> wholeEnum})$ |]
        }
    }

    where
      wholeEnum =
        [minBound..maxBound :: EGPropulsion] <&> toJSON ? \case
          x@(String y) -> (x, y)
          x -> error [qms| "declareNamedSchema" of "EGPropulsion":
                           Every constructor must be resolved to "String"
                           by using "toJSON", recieved this: {x} |]


egPropulsionToEngineIdent :: EGPropulsion -> Ident Int Engine.Engine
egPropulsionToEngineIdent Hydrogen    = Engine.hydrogen
egPropulsionToEngineIdent Electricity = Engine.electricity
egPropulsionToEngineIdent LPG         = Engine.lpg
egPropulsionToEngineIdent LNG         = Engine.lng
egPropulsionToEngineIdent Diesel      = Engine.diesel
egPropulsionToEngineIdent Gasoline    = Engine.petrol
                                          -- "Petrol"   (British English)
                                          -- "Gasoline" (American English)


egPropulsionToEngineId :: EGPropulsion -> EnginePersistent.EngineId
egPropulsionToEngineId Hydrogen    = EnginePersistent.hydrogen
egPropulsionToEngineId Electricity = EnginePersistent.electricity
egPropulsionToEngineId LPG         = EnginePersistent.lpg
egPropulsionToEngineId LNG         = EnginePersistent.lng
egPropulsionToEngineId Diesel      = EnginePersistent.diesel
egPropulsionToEngineId Gasoline    = EnginePersistent.petrol
                                       -- "Petrol"   (British English)
                                       -- "Gasoline" (American English)
