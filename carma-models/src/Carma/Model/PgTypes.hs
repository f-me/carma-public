{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Carma.Model.PgTypes where

import           Data.Int
import           Data.Maybe              ()
import           Data.Text
import           Data.Time               (Day, UTCTime)
import           Data.Time.Calendar      ()
import           Data.Vector             (Vector)
import           Data.Aeson              (Value)

import           Data.Model.Types

import           Carma.Model.LegacyTypes
import           Carma.Model.Types

instance PgTypeable t => PgTypeable (Maybe t) where
  pgTypeOf _ = PgType { pgNotNull  = False
                      , pgTypeName = pgTypeName $ pgTypeOf (undefined :: t)
                      }

instance PgTypeable t => PgTypeable (Vector t) where
  pgTypeOf _ = pgtype{ pgTypeName = pgTypeName pgtype `append` "[]" }
    where
      pgtype = pgTypeOf (undefined :: t)

instance PgTypeable t => PgTypeable (Ident t m) where
  pgTypeOf _ = pgTypeOf (undefined :: t)

instance PgTypeable Int where
  pgTypeOf _ = PgType "integer" True
instance PgTypeable Int16 where
  pgTypeOf _ = PgType "integer" True
instance PgTypeable Int32 where
  pgTypeOf _ = PgType "integer" True

instance PgTypeable Text where
  pgTypeOf _ = PgType "text" True

instance PgTypeable Bool where
  pgTypeOf _ = PgType "boolean" True

instance PgTypeable TInt where
  pgTypeOf _ = PgType "integer" True

instance PgTypeable (IdentList m) where
  pgTypeOf _ = pgTypeOf (undefined :: Vector (IdentI m))

instance PgTypeable Reference where
  pgTypeOf _ = PgType "text" True

instance PgTypeable UTCTime where
  pgTypeOf _ = PgType "timestamp with time zone" True

instance PgTypeable Day where
  pgTypeOf _ = PgType "date" True

instance PgTypeable LegacyDate where
  pgTypeOf _ = PgType "timestamp with time zone" True

instance PgTypeable LegacyDatetime where
  pgTypeOf _ = PgType "timestamp with time zone" True

instance PgTypeable (Interval UTCTime) where
  pgTypeOf _ = PgType "tstzrange" True

instance PgTypeable (Interval Day) where
  pgTypeOf _ = PgType "daterange" True

instance PgTypeable EventType where
  pgTypeOf _ = PgType "EventType" True

instance PgTypeable UserStateVal where
  pgTypeOf _ = PgType "UserStateVal" True

instance PgTypeable Value where
  pgTypeOf _ = PgType "json" True

instance PgTypeable  Activity            where pgTypeOf _ = PgType "text" True
instance PgTypeable  RequestType         where pgTypeOf _ = PgType "text" True
instance PgTypeable  ConsultationType    where pgTypeOf _ = PgType "text" True
instance PgTypeable  DeliveryType        where pgTypeOf _ = PgType "text" True
instance PgTypeable  CarClasses          where pgTypeOf _ = PgType "text" True
instance PgTypeable  DealerCities        where pgTypeOf _ = PgType "text" True
instance PgTypeable  Partner             where pgTypeOf _ = PgType "text" True
instance PgTypeable  TowerTypes          where pgTypeOf _ = PgType "text" True
instance PgTypeable  WheelsBlockedCount  where pgTypeOf _ = PgType "text" True
instance PgTypeable  UrgentServiceReason where pgTypeOf _ = PgType "text" True
instance PgTypeable  Satisfaction        where pgTypeOf _ = PgType "text" True
instance PgTypeable  FalseStatuses       where pgTypeOf _ = PgType "text" True
instance PgTypeable  CallerTypes         where pgTypeOf _ = PgType "text" True
instance PgTypeable  CallTypes           where pgTypeOf _ = PgType "text" True
instance PgTypeable  Users               where pgTypeOf _ = PgType "text" True
instance PgTypeable  JsonAsText          where pgTypeOf _ = PgType "text" True
instance PgTypeable  Phone               where pgTypeOf _ = PgType "text" True
instance PgTypeable  PickerField         where pgTypeOf _ = PgType "text" True
instance PgTypeable  MapField            where pgTypeOf _ = PgType "text" True
instance PgTypeable  Checkbox where pgTypeOf _ = PgType "boolean" True
