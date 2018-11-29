{-# LANGUAGE OverloadedStrings, QuasiQuotes, LambdaCase #-}
{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, TypeFamilies, InstanceSigs #-}

module Carma.EraGlonass.Types.EGPropulsion
     ( EGPropulsion (..)
     , egPropulsionToEngineIdent
     , egPropulsionToEngineId
     ) where

import           GHC.Generics

import           Data.Proxy
import           Data.Aeson
import           Data.Aeson.Types (Parser, typeMismatch)
import           Data.Swagger
import           Data.Swagger.Declare (Declare)
import           Text.InterpolatedString.QM
import           Data.Text (intercalate)

import           Data.Model.Types (Ident)
import qualified Carma.Model.Engine as Engine
import qualified Carma.Model.Engine.Persistent as EnginePersistent

import           Carma.Utils.StringyEnum
import           Carma.Utils.StringyEnum.SwaggerSchema
import           Carma.EraGlonass.Types.Helpers


data EGPropulsion
   = Hydrogen
   | Electricity
   | LPG
   | LNG
   | Diesel
   | Gasoline
     deriving (Eq, Enum, Bounded, Show, Generic)

instance StringyEnum EGPropulsion where
  toStringy = \case
    Hydrogen    -> "HYDROGEN"
    Electricity -> "ELECTRICITY"
    LPG         -> "LPG"
    LNG         -> "LNG"
    Diesel      -> "DIESEL"
    Gasoline    -> "GASOLINE"

instance ToJSON EGPropulsion where
  toJSON = String . toStringy

instance FromJSON EGPropulsion where
  -- | Producing list of all values to reduce human-factor mistakes,
  -- so it is handled automatically when we add a new value.
  --
  -- Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  parseJSON :: forall t. t ~ EGPropulsion => Value -> Parser t
  parseJSON jsonValue = f [minBound..(maxBound :: EGPropulsion)]
    where f [] = typeMismatch (typeName (Proxy :: Proxy t)) jsonValue
          f (x:xs) | toJSON x == jsonValue = pure x
                   | otherwise             = f xs

instance ToSchema EGPropulsion where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  declareNamedSchema
    :: forall proxy t. t ~ EGPropulsion
    => proxy t
    -> Declare (Definitions Schema) NamedSchema

  declareNamedSchema p =
    stringyEnumMappedNamedSchema p $ \x ->
      let
        schema' = _namedSchemaSchema x
        strEnum = toStringy <$> [minBound..maxBound :: t]
      in
        pure x
          { _namedSchemaSchema = schema'
              { _schemaParamSchema = (_schemaParamSchema schema')
                  { _paramSchemaFormat  = Just "propulsion"
                  , _paramSchemaPattern =
                      Just [qm| ^({intercalate "|" strEnum})$ |]
                  }
              }
          }


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
