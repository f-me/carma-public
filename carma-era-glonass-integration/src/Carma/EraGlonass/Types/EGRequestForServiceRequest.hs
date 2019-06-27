{-# LANGUAGE DeriveGeneric, DeriveAnyClass, ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, InstanceSigs #-}
{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- Fixes issue when record-fields aren't exported. Probably related to:
--   https://stackoverflow.com/questions/46357747/haddock-data-record-fields-names-not-being-generated
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | There's no response type here because by spec we're supposed to return
--   empty response body with 200 status in case everything is okay.
module Carma.EraGlonass.Types.EGRequestForServiceRequest
     ( EGRequestForServiceRequest         (..)
     , EGRequestForServiceRequestLocation (..)
     , EGRequestForServiceRequestVehicle  (..)

     , proof
     ) where

import           GHC.Generics

import           Data.Proxy
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.HashMap.Lazy as HM
import           Data.Aeson hiding (json)
import           Data.Aeson.Types (Parser, parseEither)
import           Data.Swagger
import           Data.Swagger.Internal.Schema
import           Data.Swagger.Declare (Declare)

import           Database.PostgreSQL.Simple.FromField
                   ( FromField (..)
                   , fromJSONField
                   )
import           Database.PostgreSQL.Simple.ToField
                   ( ToField (..)
                   , toJSONField
                   )
import           Database.Persist.Postgresql.JSON ()
import           Database.Persist.Sql (PersistFieldSql (sqlType))
import           Database.Persist.Class
                   ( PersistField (toPersistValue, fromPersistValue)
                   , fromPersistValueJSON
                   )

import           Data.Model (fieldName, fieldDesc)

import           Data.Model.Types
                   ( PgTypeable (pgTypeOf)
                   , PgType (PgType)
                   , DefaultFieldView (defaultFieldView)
                   , FieldView (..)
                   , FF
                   , fieldKindStr
                   )

import           Carma.Utils.Operators
import           Carma.Utils.StringyEnum
import           Carma.Utils.StringyEnum.Aeson
import           Carma.Utils.StringyEnum.SwaggerSchema
import           Carma.Utils.TypeSafe.Proxy
import           Carma.Utils.TypeSafe.Generic.Aeson
import           Carma.Utils.TypeSafe.Generic.Record hiding (fieldName)
import           Carma.Utils.TypeSafe.Generic.DataType.Operations.RemoveConstructor
import           Carma.EraGlonass.Types.Helpers.Proof
import           Carma.EraGlonass.Types.Helpers.NonEmptyText
import           Carma.EraGlonass.Types.EGRequestId (EGRequestId)
import           Carma.EraGlonass.Types.EGDateTime (EGDateTime)
import           Carma.EraGlonass.Types.EGVin (EGVin)
import qualified Carma.EraGlonass.Types.EGLatLon as EGLatLon


proof :: ()
proof
  = proofThatTheTypeIsComplete (Proxy :: Proxy EGRequestForServiceRequest)
  ! proofThatTheTypeIsComplete
      (Proxy :: Proxy EGRequestForServiceRequestLocation)
  ! proofThatTheTypeIsComplete
      (Proxy :: Proxy EGRequestForServiceRequestVehicle)


-- | First \"Request" is part of name of action and last ending \"Request" means
--   it's type of HTTP request body, not response.
data EGRequestForServiceRequest
   -- | A constructor for failure case to be able to handle incorrect request
   --   in a more flexible way (log it, store incorrect request for debugging
   --   purposes, etc.).
   = EGRequestForServiceRequestIncorrect
   { errorMessage :: String
       -- ^ Some error message, like failure message from parser.

   , incorrectRequestBody :: Value
       -- ^ Original provided JSON which is failed to be parsed.
   }

   -- | Successful constructor
   | EGRequestForServiceRequest
   { requestId :: EGRequestId
       -- ^ Unique identity of a request for service.

   , serviceCategoryId :: NonEmptyText
       -- ^ This a category of service which is chosen by EG's operator.
       --
       -- We probably don't need this value for our internal purposes.

   , statusCode :: EGRequestForServiceStatusCode
       -- ^ In this particular integration point it can only be @\"SENT"@.
       --
       -- Other integration points have the same field with different possible
       -- values.

   , statusTime :: EGDateTime
       -- ^ Last status update of this request for service.
       --
       -- I believe it may be earlier than time when request for service is
       -- delivered to us from EG.

   , ivsPhoneNumber :: Maybe NonEmptyText
       -- ^ Phone number of a terminal integrated to a vehicle.

   , fullName :: Maybe NonEmptyText
       -- ^ Surname, first name and middle name of a user of service (caller).

   , phoneNumber :: Maybe NonEmptyText
       -- ^ Phone number of a user of service (caller).

   , location :: EGRequestForServiceRequestLocation
       -- ^ Current coordinates of a user of service (caller).

   , deferTime :: Maybe EGDateTime
       -- ^ In case it's a delayed service - time when the service should be
       --   provided.

   , vehicle :: Maybe EGRequestForServiceRequestVehicle
       -- ^ Info about vehicle (VIN and optional plate number).

   , fccComment :: Maybe NonEmptyText
       -- ^ An optional comment by an operator of call center.
   }

     deriving (Eq, Show, Generic)

instance FromJSON EGRequestForServiceRequest where
  parseJSON
    :: forall t final okConstructor
     .
     ( t ~ EGRequestForServiceRequest
     , okConstructor ~ "EGRequestForServiceRequest"

     , 'Just final ~
         RemoveConstructorByName (Rep t) "EGRequestForServiceRequestIncorrect"
     )
    => Value
    -> Parser t

  parseJSON src = pure go where
    -- | Parsing here to extract parsing error message.
    go = case parseEither (const successfulCase) src of
              Left msg -> EGRequestForServiceRequestIncorrect msg src
              Right x  -> x

    okConstructorProxy = Proxy :: Proxy '(final, okConstructor)

    successfulCase = do
      obj <-
        -- Handling of optional @NonEmptyText@ fields
        preParseOptionalNonEmptyTextFieldsRep'
          (proxyPair2Triplet okConstructorProxy (Proxy :: Proxy 4)) src

      -- Associating it with default constructor for successful case
      genericParseJSON defaultOptions $ Object $
        addConstructorTag' okConstructorProxy $ obj

instance ToJSON EGRequestForServiceRequest where
  toJSON :: forall t. t ~ EGRequestForServiceRequest => t -> Value

  toJSON
    EGRequestForServiceRequestIncorrect
      { errorMessage, incorrectRequestBody } = object result where

    tConstructorProxy =
      Proxy :: Proxy '(t, "EGRequestForServiceRequestIncorrect")

    result =
      [ "status" .= ("error" :: Text)

      , constructorFieldName
          (proxyPair2Triplet tConstructorProxy
                             (Proxy :: Proxy "errorMessage")) .= errorMessage

      , constructorFieldName
          (proxyPair2Triplet tConstructorProxy
                             (Proxy :: Proxy "incorrectRequestBody")) .=
          incorrectRequestBody
      ]

  toJSON x@EGRequestForServiceRequest {} =
    case genericToJSON defaultOptions x of
         Object obj -> Object $ HM.delete "tag" obj
         json -> error $
           "Impossible value (it's supposed to be an Object): " <> show json

instance ToSchema EGRequestForServiceRequest where
  -- | Cutting off failure constructor.
  declareNamedSchema
    :: forall proxy t final
     .
     ( t ~ EGRequestForServiceRequest

     , -- We don't need failure constructor in our spec
       -- since it's for internal use only.
       'Just final ~
         RemoveConstructorByName (Rep t) "EGRequestForServiceRequestIncorrect"
     )
    => proxy t
    -> Declare (Definitions Schema) NamedSchema

  declareNamedSchema _ =
    gdeclareNamedSchema defaultSchemaOptions (Proxy :: Proxy final) mempty

instance PersistField EGRequestForServiceRequest where
  toPersistValue = toPersistValue . toJSON
  fromPersistValue = fromPersistValueJSON

instance PersistFieldSql EGRequestForServiceRequest where
  sqlType Proxy = sqlType (Proxy :: Proxy Value)

instance PgTypeable EGRequestForServiceRequest where
  pgTypeOf _ = PgType "json" True

instance DefaultFieldView EGRequestForServiceRequest where
  defaultFieldView f
    = FieldView
    { fv_name = fieldName f
    , fv_type = "JSON"
    , fv_canWrite = False
    , fv_meta =
        [ ("label", String $ fieldDesc f)
        , ("app",   String $ fieldKindStr $ fieldToFieldKindProxy f)
        ]
    } where fieldToFieldKindProxy :: (m -> FF t nm desc a) -> Proxy a
            fieldToFieldKindProxy _ = Proxy

instance FromField EGRequestForServiceRequest where
  fromField = fromJSONField

instance ToField EGRequestForServiceRequest where
  toField = toJSONField


data EGRequestForServiceRequestLocation
   = EGRequestForServiceRequestLocation
   { latitude    :: EGLatLon.EGLatitude
   , longitude   :: EGLatLon.EGLongitude
   , description :: Maybe NonEmptyText
   }
     deriving (Eq, Show, Generic, ToJSON, ToSchema)

instance FromJSON EGRequestForServiceRequestLocation where
  parseJSON
    :: forall t c n p
     .
     ( t ~ EGRequestForServiceRequestLocation
     , c ~ "EGRequestForServiceRequestLocation"
     , n ~ 1 -- Count of found @Maybe NonEmptyText@ fields
     , p ~ '(t, c, n)
     )
    => Value
    -> Parser t

  parseJSON = parseJSONWithOptionalNonEmptyTextFields' (Proxy :: Proxy p)


data EGRequestForServiceRequestVehicle
   = EGRequestForServiceRequestVehicle
   { vin :: EGVin
   , plateNumber :: Maybe NonEmptyText
   }
     deriving (Eq, Show, Generic, ToJSON, ToSchema)

instance FromJSON EGRequestForServiceRequestVehicle where
  parseJSON
    :: forall t c n p
     .
     ( t ~ EGRequestForServiceRequestVehicle
     , c ~ "EGRequestForServiceRequestVehicle"
     , n ~ 1 -- Count of found @Maybe NonEmptyText@ fields
     , p ~ '(t, c, n)
     )
    => Value
    -> Parser t

  parseJSON = parseJSONWithOptionalNonEmptyTextFields' (Proxy :: Proxy p)


data EGRequestForServiceStatusCode = Sent
     deriving (Eq, Show, Enum, Bounded, Generic)

instance StringyEnum EGRequestForServiceStatusCode where
  toStringy Sent = "SENT"

instance ToJSON EGRequestForServiceStatusCode where
  toJSON = String . toStringy

instance FromJSON EGRequestForServiceStatusCode where
  parseJSON = parseStringyEnumJSON

instance ToSchema EGRequestForServiceStatusCode where
  declareNamedSchema = stringyEnumNamedSchema
