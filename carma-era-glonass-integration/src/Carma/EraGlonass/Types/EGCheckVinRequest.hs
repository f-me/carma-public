{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- Fixes issue when record-fields aren't exported. Probably related to:
--   https://stackoverflow.com/questions/46357747/haddock-data-record-fields-names-not-being-generated
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Data-types for checking if VIN is handled by CaRMa (__CRM.EG.02__)
-- request and response.
module Carma.EraGlonass.Types.EGCheckVinRequest
     ( EGCheckVinRequest (..)
     , EGCheckVinRequestRequests (..)
     , EGCheckVinResponse (..)
     , EGCheckVinResponseResponses (..)
     , EGCheckVinResponseVinProviders (..)
     ) where

import           GHC.Generics
import           GHC.TypeLits

import           Data.Proxy
import           Data.Text (Text)
import           Data.String (fromString)
import           Data.Aeson
import           Data.Aeson.TH (Options (omitNothingFields))
import           Data.Aeson.Types (Parser, typeMismatch, parseEither)
import           Data.Swagger
import           Data.Swagger.Internal.Schema
import           Data.Swagger.Declare (Declare)

import           Carma.Utils.Operators
import           Carma.Utils.TypeSafe.Generic.DataType
import           Carma.Utils.TypeSafe.Generic.Record
import           Carma.Utils.TypeSafe.Generic.Aeson
import           Carma.EraGlonass.Types.RequestId (RequestId)
import           Carma.EraGlonass.Types.EGVin (EGVin)


-- *** Request


data EGCheckVinRequest
   = EGCheckVinRequest
   { requestId :: RequestId
   , requests :: [EGCheckVinRequestRequests]
   } deriving (Eq, Show, Generic, ToSchema)

instance ToJSON EGCheckVinRequest where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }


data EGCheckVinRequestRequests
   = EGCheckVinRequestRequests
   { vin :: EGVin
   } deriving (Eq, Show, Generic)

instance ToJSON EGCheckVinRequestRequests where
  toJSON (EGCheckVinRequestRequests (vin' :: EGVin)) =
    object [fromString (symbolVal (Proxy :: Proxy VinFieldName)) .= vin']

instance ToSchema EGCheckVinRequestRequests where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  declareNamedSchema
    :: forall proxy t t2
    .  ( t ~ EGCheckVinRequestRequests
       , t2 ~ UppercaseVinField (Rep t)
       )
    => proxy t
    -> Declare (Definitions Schema) NamedSchema

  declareNamedSchema _ =
    gdeclareNamedSchema defaultSchemaOptions (Proxy :: Proxy t2) mempty

type family UppercaseVinField (k1 :: * -> *) where
  UppercaseVinField (D1 a (C1 x field)) =
    D1 a (C1 x (ReplaceFieldKey "vin" VinFieldName field))

type VinFieldName = "VIN"


-- *** Response


data EGCheckVinResponse
   = EGCheckVinResponseIncorrect
   { errorMessage :: String
   , incorrectResponseBody :: Value
   }

   | EGCheckVinResponse
   { requestId :: RequestId
   , responses :: [EGCheckVinResponseResponses]
   }
     deriving (Eq, Show, Generic)

instance FromJSON EGCheckVinResponse where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  parseJSON :: forall t. t ~ EGCheckVinResponse => Value -> Parser t
  parseJSON src = pure $
    -- Parsing here to extract parsing error message
    case parseEither (const successfulCase) src of
         Left msg -> EGCheckVinResponseIncorrect msg src
         Right x  -> x

    where
      typeName'' = typeName (Proxy :: Proxy t)

      okConstructorProxy :: Proxy '(t, "EGCheckVinResponse")
      okConstructorProxy = Proxy

      successfulCase = do
        obj <- -- Extracting hash-map from JSON @Object@
          case src of
               Object x -> pure x
               _        -> typeMismatch typeName'' src

        genericParseJSON defaultOptions $
          -- Associating it with successful case constructor
          Object $ addConstructorTag okConstructorProxy obj

type FailureConsMeta
   = 'MetaCons "EGCheckVinResponseIncorrect" 'PrefixI 'True

-- | Slicing 'EGCheckVinResponseIncorrect' constructor from Swagger spec.
type family CutOffFailureCons (k1 :: * -> *) where
  CutOffFailureCons (D1 a (C1 FailureConsMeta _ :+: y)) = D1 a y

instance ToSchema EGCheckVinResponse where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  declareNamedSchema
    :: forall proxy t t2
    .  ( t ~ EGCheckVinResponse
       , t2 ~ CutOffFailureCons (Rep t)
       )
    => proxy t
    -> Declare (Definitions Schema) NamedSchema

  declareNamedSchema _ =
    gdeclareNamedSchema defaultSchemaOptions (Proxy :: Proxy t2) mempty


-- | List item type of "responses" field of "EGCheckVinResponse"
data EGCheckVinResponseResponses
   = EGCheckVinResponseResponsesVinExists
   { vin :: EGVin
   , vinProviders :: [EGCheckVinResponseVinProviders]
   } -- ^ When @\"vinStatus"@ is @true@ (JSON)

   | EGCheckVinResponseResponsesVinNotExists
   { vin :: EGVin
   } -- ^ When @\"vinStatus"@ is @false@ (JSON)

     deriving (Eq, Show, Generic)

instance FromJSON EGCheckVinResponseResponses where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  parseJSON
    :: forall t t2
    .  ( t ~ EGCheckVinResponseResponses
       , t2 ~ AddEphemeralVinStatusField (Rep t)
       )
    => Value
    -> Parser t

  parseJSON src = do
    obj <- case src of
      Object x -> pure x
      _        -> mismatch

    vinStatus <-
      either (const mismatch) pure $
        getFieldValue' (Proxy :: Proxy '(t2, "vinStatus")) obj

    genericParseJSON defaultOptions $
      Object $ (obj &) $
        if vinStatus
           then addConstructorTag (Proxy :: Proxy
                  '(t, "EGCheckVinResponseResponsesVinExists"))
           else addConstructorTag (Proxy :: Proxy
                  '(t, "EGCheckVinResponseResponsesVinNotExists"))
    where
      mismatch :: Parser a -- Making it polymorphic
      mismatch = typeMismatch (typeName (Proxy :: Proxy t)) src

instance ToSchema EGCheckVinResponseResponses where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  declareNamedSchema
    :: forall proxy t t2
    .  ( t ~ EGCheckVinResponseResponses
       , t2 ~ AddEphemeralVinStatusField (Rep t)
       )
    => proxy t
    -> Declare (Definitions Schema) NamedSchema

  declareNamedSchema _ =
    gdeclareNamedSchema defaultSchemaOptions (Proxy :: Proxy t2) mempty

type family AddEphemeralVinStatusField (k1 :: * -> *) where
  AddEphemeralVinStatusField
    (D1 a (C1 VinExistsMetaCons f1 :+: C1 VinNotExistsMetaCons f2)) =
      D1 a (   C1 VinExistsMetaCons    (VinStatusField :*: f1)
           :+: C1 VinNotExistsMetaCons (VinStatusField :*: f2)
           )

type VinExistsMetaCons
   = 'MetaCons "EGCheckVinResponseResponsesVinExists" 'PrefixI 'True
type VinNotExistsMetaCons
   = 'MetaCons "EGCheckVinResponseResponsesVinNotExists" 'PrefixI 'True
type VinStatusFieldName = "vinStatus"
type VinStatusField
   = S1 ( 'MetaSel ('Just VinStatusFieldName)
                   'NoSourceUnpackedness
                   'NoSourceStrictness
                   'DecidedLazy
        ) (Rec0 Bool)


data EGCheckVinResponseVinProviders
   = EGCheckVinResponseVinProviders
   { code :: Text
   , title :: Text
   } deriving (Eq, Show, Generic, FromJSON, ToSchema)
