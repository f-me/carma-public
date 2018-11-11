{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

-- Fixes issue when record-fields aren't exported. Probably related to:
--   https://stackoverflow.com/questions/46357747/haddock-data-record-fields-names-not-being-generated
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Data-types for __EG.CRM.01__ request and response.
module Carma.EraGlonass.Types.EGCreateCallCardRequest
     ( EGCreateCallCardRequest (..)
     , EGCreateCallCardRequestGis (..)
     , EGCreateCallCardRequestVehicle (..)
     , EGCreateCallCardResponse (..)
     ) where

import           GHC.Generics

import           Data.Proxy
import qualified Data.HashMap.Lazy as HM
import           Data.Text (Text, length)
import           Data.Aeson
import           Data.Aeson.TH (Options (omitNothingFields))
import           Data.Aeson.Types (typeMismatch, parseEither)
import           Data.Swagger
import           Data.Swagger.Internal.Schema
import           Text.InterpolatedString.QM

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

import           Data.Model
import           Data.Model.Types

import           Carma.Model.Case.Persistent (Case)
import           Carma.EraGlonass.Types.RequestId (RequestId)
import           Carma.EraGlonass.Types.EGPhoneNumber (EGPhoneNumber)
import qualified Carma.EraGlonass.Types.EGLatLon as EGLatLon
import           Carma.EraGlonass.Types.EGCallCardId (EGCallCardId)
import           Carma.EraGlonass.Types.EGCallerFullName (EGCallerFullName)
import           Carma.EraGlonass.Types.EGVin (EGVin)
import           Carma.EraGlonass.Types.EGPropulsion
import           Carma.EraGlonass.Types.PersistentTextKey (PersistentTextKey)
import           Carma.EraGlonass.Types.EGCreateCallCardAcceptCode
                   ( EGCreateCallCardAcceptCode
                   )


-- *** Request ***


-- | Request body data-type of __EG.CRM.01__.
data EGCreateCallCardRequest
   -- | A constructor for failure case to be able to handle incorrect request
   --   in a more flexible way (log it, store incorrect request for debugging
   --   purposes, etc.).
   = EGCreateCallCardRequestIncorrect
   { errorMessage :: String
       -- ^ Some error message, like failure message from parser.

   , incorrectRequestBody :: Value
       -- ^ Original provided JSON which is failed to be parsed.
   }

   -- | Successful constructor
   | EGCreateCallCardRequest
   { requestId :: RequestId
       -- ^ Unique request identifier (required to answer).
       --
       -- We also have been told it is an UUID.
       --
       -- Read about UUID here:
       --   https://en.wikipedia.org/wiki/Universally_unique_identifier
       --
       -- CaRMa field:
       --   'Carma.EraGlonass.Model.CaseEraGlonassCreateRequest.Persistent.caseEraGlonassCreateRequestRequestId'

   , cardIdCC :: EGCallCardId
       -- ^ Identity of \"Call Card" (required to answer).
       --
       -- Just a string, documentation says it's unsigned integer but in an
       -- example it is @\"597b53edf0f012e5e00d8a9a"@, after clarifying we know
       -- it is a free string value.
       --
       -- CaRMa field:
       --   'Carma.EraGlonass.Model.CaseEraGlonassCreateRequest.Persistent.caseEraGlonassCreateRequestCallCardId'

   , atPhoneNumber :: EGPhoneNumber
       -- ^ Phone number of a car emergency terminal
       --   (embedded SIM card or something like that).
       --
       -- A string from 1 to 18 chars of numbers
       -- which could be prefixed with @+@.
       --
       -- CaRMa field: @contact_phone2@

   , lastTrustedLatitude :: EGLatLon.EGLatitude
       -- ^ An integer in range of -648000000 .. 648000000.
       --
       -- CaRMa field: @caseAddress_coords@ (alongwith /@lastTrustedLongitude@/)

   , lastTrustedLongitude :: EGLatLon.EGLongitude
       -- ^ An integer in range of -324000000 .. 324000000.
       --
       -- CaRMa field: @caseAddress_coords@ (alongwith /@lastTrustedLatitude@/)

   , callerFullName :: EGCallerFullName
       -- ^ A string of 50 chars.
       --
       -- CaRMa field: @contact_name@

   , callerPhoneNumber :: EGPhoneNumber
       -- ^ A phone number of a customer, his personal phone number,
       --   where /@atPhoneNumber@/ is phone number of a car emergency terminal.
       --
       -- A string from 1 to 18 chars of numbers
       -- which could be prefixed with @+@.
       --
       -- CaRMa field: @contact_phone1@

   , locationDescription :: Text
       -- ^ String with max length of 180 symbols.
       --
       -- CaRMa field: @caseAddress_comment@

   , vehicle :: EGCreateCallCardRequestVehicle
   , gis :: [EGCreateCallCardRequestGis]
   } deriving (Eq, Show, Generic)

-- | Handling multiple constructors here.
--
-- 'EGCreateCallCardRequest' is default successful case
-- but if JSON source is incorrect we just constructing
-- 'EGCreateCallCardRequestIncorrect' and taking is it as correctly parsed
-- keeping parse failure message and original JSON data in failure
-- constructor.
instance FromJSON EGCreateCallCardRequest where
  parseJSON src = pure $
    -- Parsing here to extract parsing error message
    case parseEither (const successfulCase) src of
         Left msg -> EGCreateCallCardRequestIncorrect msg src
         Right x  -> x

    where
      successfulCase = do
        -- Extracting hash-map from JSON @Object@
        obj <- case src of
                    Object x -> pure x
                    _        -> typeMismatch "EGCreateCallCardRequest" src

        parsed <- genericParseJSON defaultOptions $
          -- Associating it with default constructor for successful case
          Object $ HM.insert "tag" (String "EGCreateCallCardRequest") obj

        -- Handling max length of /@locationDescription@/
        if Data.Text.length (locationDescription parsed) > 180
           then typeMismatch "EGCreateCallCardRequest.locationDescription" src
           else pure ()

        pure parsed

instance ToJSON EGCreateCallCardRequest where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

type FailureConsMeta
   = 'MetaCons "EGCreateCallCardRequestIncorrect" 'PrefixI 'True

-- | Slicing 'EGCreateCallCardRequestIncorrect' constructor from Swagger spec.
type family CutOffFailureCons (k1 :: * -> *) where
  CutOffFailureCons (D1 a (C1 FailureConsMeta _ :+: y)) = D1 a y

-- | Generic implementation with cutting off 'EGCreateCallCardRequestIncorrect'
--   constructor from type-level type representation (@Rep@).
instance ToSchema EGCreateCallCardRequest where
  declareNamedSchema _ = gdeclareNamedSchema defaultSchemaOptions proxy mempty
    where proxy :: Proxy (CutOffFailureCons (Rep EGCreateCallCardRequest))
          proxy = Proxy

instance PersistField EGCreateCallCardRequest where
  toPersistValue = toPersistValue . toJSON
  fromPersistValue = fromPersistValueJSON

instance PersistFieldSql EGCreateCallCardRequest where
  sqlType Proxy = sqlType (Proxy :: Proxy Value)

instance PgTypeable EGCreateCallCardRequest where
  pgTypeOf _ = PgType "json" True

instance DefaultFieldView EGCreateCallCardRequest where
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

instance FromField EGCreateCallCardRequest where
  fromField = fromJSONField

instance ToField EGCreateCallCardRequest where
  toField = toJSONField


-- | /\"gis"/ 'EGCreateCallCardRequest' branch data type
data EGCreateCallCardRequestGis
   = EGCreateCallCardRequestGis
   { regionName :: Text
       -- CaRMa field: @caseAddress_address@
   , settlementName :: Text
       -- CaRMa field: @caseAddress_address@
   , streetName :: Text
       -- CaRMa field: @caseAddress_address@
   , building :: Text
       -- ^ Number of a building as a string.
       --
       -- CaRMa field: @caseAddress_address@
   } deriving (Eq, Show, Generic, ToSchema, FromJSON)

instance ToJSON EGCreateCallCardRequestGis where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }


-- | /\"vehicle"/ 'EGCreateCallCardRequest' branch data type
data EGCreateCallCardRequestVehicle
   = EGCreateCallCardRequestVehicle
   { vin :: EGVin
       -- ^ A car's VIN (\"Alphanumeric").
       --
       -- We expect it to have length of 17 alphanumeric chars
       -- excluding \"I", \"O" and \"Q" as declared in VIN spec.
       --
       -- See also:
       --   https://en.wikipedia.org/wiki/Vehicle_identification_number
       --
       -- CaRMa field: /@contractIdentifier@/ and /@car_vin@/ (just duplicate),
       --              see comments for these fields in
       --              'Carma.Model.Case.Persistent.Case' model for details.

   , propulsion :: Maybe EGPropulsion
       -- ^ Enum of:
       --     @\"HYDROGEN"@;
       --     @\"ELECTRICITY"@;
       --     @\"LPG"@ - Liquefied Petroleum Gas (propane);
       --     @\"LNG"@ - Liquefied Natural Gas;
       --     @\"DIESEL"@;
       --     @\"GASOLINE"@.
       --
       -- Looking at the example I realized this field is optional
       -- (since empty string is presented for @propulsion@,
       -- so I realized it is possible, and empty string is taken
       -- as @Nothing@).
       --
       -- CaRMa field: @car_engine@

   , color :: Text
       -- ^ A color of a car (string with max length of 50 symbols).
       --
       -- CaRMa field: @car_color@

   , registrationNumber :: Text
       -- CaRMa field: @car_plateNum@
   } deriving (Eq, Show, Generic, ToSchema)

instance FromJSON EGCreateCallCardRequestVehicle where
  parseJSON src@(Object rootObj) = do
    parsed <-
      genericParseJSON defaultOptions $
        -- Interpreting empty string of @propulsion@ as @Nothing@
        case HM.lookup "propulsion" rootObj of
             Just (String "") -> Object $ HM.delete "propulsion" rootObj
             _ -> src

    -- Handling max length of @color@
    if Data.Text.length (color parsed) > 50
       then typeMismatch "EGCreateCallCardRequestVehicle.color" src
       else pure ()

    pure parsed

  parseJSON x = typeMismatch "EGCreateCallCardRequestVehicle" x

instance ToJSON EGCreateCallCardRequestVehicle where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }


-- *** Response ***


-- | __EG.CRM.01__ response data-type.
data EGCreateCallCardResponse
   -- | Successful constructor
   = EGCreateCallCardResponse
   { responseId :: Text
       -- ^ We're supposed to form it by ourselves.
       --
       --   It can be just random string.

   , cardidProvider :: PersistentTextKey Case
       -- ^ CaRMa 'Carma.Model.Case.Persistent.Case' id.

   , acceptId :: EGCallCardId
       -- ^ It supposed to be equal to obtained from
       --   'EGCreateCallCardRequest'./@cardIdCC@/,
       --   just getting it from request and putting it here.

   , requestId :: RequestId
       -- ^ It supposed to be equal to obtained from
       --   'EGCreateCallCardRequest'./@requestId@/,
       --   just getting it from request and putting it here.

   , acceptCode :: EGCreateCallCardAcceptCode
       -- ^ Possible values:
       --     @\"OK"@               - If data successfully accepted;
       --     @\"INCORRECT_FORMAT"@ - If accepted data is incorrect;
       --     @\"INTERNAL_ERROR"@   - Internal CaRMa-side error.

   , statusDescription :: Maybe Text
       -- ^ It's a free string, just a meta information, could be @\"OK"@ or an
       --   error's stack trace which would help to debug stuff.
   }

   | EGCreateCallCardResponseFailure
   { responseId        :: Text
   , acceptCode        :: EGCreateCallCardAcceptCode
   , statusDescription :: Maybe Text
   }

     deriving (Eq, Show, Generic, ToSchema)

instance ToJSON EGCreateCallCardResponse where
  toJSON = f . genericToJSON defaultOptions { omitNothingFields = True }
    where f (Object rootObj) = Object $ HM.delete "tag" rootObj
          f x = error [qms| ToJSON EGCreateCallCardResponse:
                            Unexpected root JSON type: {x} |]
