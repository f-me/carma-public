{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

-- | @Partner@ persistent model.
module Carma.Model.Partner.Persistent where

import           Data.Text (Text)
import           Data.Typeable
import           Data.Aeson (Value)
import           Data.Time.Clock (UTCTime)

import           Database.Persist.TH

import           Database.Persist.Postgresql.JSON ()
                 -- ^ @PersistFieldSql Value@ instance

import           Carma.Model.Types (Coords)
import           Carma.Model.City.Persistent (CityId)
import           Carma.Model.CarMake.Persistent (CarMakeId)
import           Carma.Model.TaxScheme.Persistent (TaxSchemeId)


-- | @Partner@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Partner json sql=partnertbl
  isActive Bool sql=isactive
  isDealer Bool sql=isdealer
  isMobile Bool sql=ismobile
  isFree   Bool sql=isfree

  name Text sql=name
  synonyms [Text] sql=synonyms
  code Text Maybe sql=code
  city CityId Maybe sql=city
  makes [CarMakeId] sql=makes
  services Value sql=services
  phones Value sql=phones
  coords Coords Maybe sql=coords
  addrs Value sql=addrs
  emails Value sql=emails
  personInCharge Text Maybe sql=personincharge
  taxScheme TaxSchemeId Maybe sql=taxscheme
  isPayBackConfirmed Bool sql=ispaybackconfirmed
  foreignIdent Text Maybe sql=foreignident
  mtime UTCTime sql=mtime
  comment Text sql=comment

  deriving Typeable Show
|]
