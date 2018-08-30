{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Carma.EraGlonass.Model.CaseEraGlonassFailures.Types
     ( EGIntegrationPoint (..)
     ) where

import           Data.Proxy
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)

import           Database.PostgreSQL.Simple.FromField
                   ( FromField (..)
                   , fromJSONField
                   )
import           Database.PostgreSQL.Simple.ToField
                   ( ToField (..)
                   , toJSONField
                   )
import           Database.Persist.Sql (PersistFieldSql (sqlType))
import           Database.Persist.Types (SqlType (..))
import           Database.Persist.Class
                   ( PersistField (toPersistValue, fromPersistValue)
                   , toPersistValueJSON
                   , fromPersistValueJSON
                   )

import           Data.Model
import           Data.Model.Types


data EGIntegrationPoint
   = EgCrm01 -- ^ "EG.CRM.01"
   | CrmEg02 -- ^ "CRM.EG.02"
   | CrmEg03 -- ^ "CRM.EG.03"
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
  toJSON EgCrm01 = String "EG.CRM.01"
  toJSON CrmEg02 = String "CRM.EG.02"
  toJSON CrmEg03 = String "CRM.EG.03"

instance FromField EGIntegrationPoint where
  fromField = fromJSONField

instance ToField EGIntegrationPoint where
  toField = toJSONField

instance PersistField EGIntegrationPoint where
  toPersistValue = toPersistValueJSON
  fromPersistValue = fromPersistValueJSON

instance PersistFieldSql EGIntegrationPoint where
  sqlType Proxy = SqlString
