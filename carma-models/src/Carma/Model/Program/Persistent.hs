{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.Program.Persistent where

import           GHC.Generics (Generic)

import           Data.Typeable
import           Data.Text (Text)

import           Database.Persist.Sql (toSqlKey)
import           Database.Persist.TH

import           Carma.Model.Usermeta.Persistent (UsermetaId)
import           Carma.Model.ProgramType.Persistent (ProgramTypeId)


-- | @Program@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Program sql=Program
  active Bool sql=active
  label Text sql=label
  shortLabel Text Maybe sql=shortlabel
  logo Text Maybe sql=logo
  client Text Maybe sql=client
  clientAddress Text Maybe sql=clientaddress
  clientCode Text Maybe sql=clientcode
  fdds Text Maybe sql=fdds
  managers [UsermetaId] Maybe sql=managers
  pType ProgramTypeId Maybe sql=ptype
  help Text Maybe sql=help

  deriving Typeable Generic Show
|]


-- | @Program@ predefined IDs.
arc, atlant, avilon, b2c, citroen, euro, ford, genser, gm, nz :: ProgramId
mapfre, peugeot, ramc, ruslan, unicredit, vnukovo, vtb24, vw, alarm :: ProgramId
arc = toSqlKey 30
atlant = toSqlKey 11
avilon = toSqlKey 18
b2c = toSqlKey 15
citroen = toSqlKey 67
euro = toSqlKey 9
ford = toSqlKey 6
genser = toSqlKey 32
gm = toSqlKey 4
nz = toSqlKey 17
mapfre = toSqlKey 7
peugeot = toSqlKey 1
ramc = toSqlKey 64
ruslan = toSqlKey 16
unicredit = toSqlKey 47
vnukovo = toSqlKey 19
vtb24 = toSqlKey 2
vw = toSqlKey 3
alarm = toSqlKey 5
