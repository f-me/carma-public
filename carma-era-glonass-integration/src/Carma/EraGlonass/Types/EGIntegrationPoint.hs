{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Carma.EraGlonass.Types.EGIntegrationPoint
     ( EGIntegrationPoint (..)
     ) where

import           Data.Proxy
import           Data.String (fromString)
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Text.InterpolatedString.QM
import           Data.Text.Encoding (encodeUtf8)

import           Database.PostgreSQL.Simple.FromField
                   ( FromField (..)
                   , fromJSONField
                   )
import           Database.PostgreSQL.Simple.ToField
                   ( ToField (..)
                   , toJSONField
                   )
import           Database.Persist.Sql (PersistFieldSql (sqlType))
import           Database.Persist.Types (PersistValue (..), SqlType (..))
import           Database.Persist.Class
                   ( PersistField (toPersistValue, fromPersistValue)
                   )

import           Data.Model
import           Data.Model.Types


-- | Be careful changing constructors here,
--   automatically derived @Show@ instance is used to match SQL ENUM.
data EGIntegrationPoint
   = RequestForService
   | BindVehicles
   | ChangeProcessingStatus
   | ChangeRequestStatus
     deriving (Show, Eq, Enum, Bounded)

instance PgTypeable EGIntegrationPoint where
  pgTypeOf _ = PgType "text" True

instance DefaultFieldView EGIntegrationPoint where
  defaultFieldView f
    = FieldView
    { fv_name = fieldName f
    , fv_type = "text"
    , fv_canWrite = False
    , fv_meta =
        [ ("label", String $ fieldDesc f)
        , ("app",   String $ fieldKindStr $ fieldToFieldKindProxy f)
        ]
    } where fieldToFieldKindProxy :: (m -> FF t nm desc a) -> Proxy a
            fieldToFieldKindProxy _ = Proxy

instance FromJSON EGIntegrationPoint where
  -- Producing list of all values to reduce human-factor mistakes,
  -- so it is handled automatically when we add a new value.
  parseJSON jsonValue = f [minBound..(maxBound :: EGIntegrationPoint)]
    where f [] = typeMismatch "EGIntegrationPoint" jsonValue
          f (x:xs) | toJSON x == jsonValue = pure x
                   | otherwise             = f xs

instance ToJSON EGIntegrationPoint where
  toJSON = String . fromString . show

instance FromField EGIntegrationPoint where
  fromField = fromJSONField

instance ToField EGIntegrationPoint where
  toField = toJSONField

instance PersistField EGIntegrationPoint where
  toPersistValue = PersistDbSpecific . fromString . show

  -- Producing list of all values to reduce human-factor mistakes,
  -- so it is handled automatically when we add a new value.
  fromPersistValue x@(PersistDbSpecific _) =
    f [minBound..(maxBound :: EGIntegrationPoint)]
    where f [] = Left [qm| Expected EGIntegrationPoint, received: {x} |]
          f (z:zs) | toPersistValue z == x = pure z
                   | otherwise             = f zs
  fromPersistValue (PersistText x) =
    fromPersistValue $ PersistDbSpecific $ encodeUtf8 x
  fromPersistValue x =
    Left [qms| Expected PersistDbSpecific for EGIntegrationPoint,
               received: {x} |]

instance PersistFieldSql EGIntegrationPoint where
  sqlType Proxy = SqlString
