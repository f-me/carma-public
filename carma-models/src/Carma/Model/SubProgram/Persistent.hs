{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.SubProgram.Persistent where

import           Data.Text (Text)
import           Data.Typeable

import           Database.Persist.TH
import           Database.Persist.Sql (toSqlKey)

import           Carma.Model.LegacyTypes (Reference)

import           Carma.Model.Program.Persistent (ProgramId)
import           Carma.Model.DiagSlide.Persistent (DiagSlideId)
import           Carma.Model.CarMake.Persistent (CarMakeId)
import           Carma.Model.ServiceType.Persistent (ServiceTypeId)


-- | @SubProgram@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
SubProgram sql=SubProgram
  parent ProgramId sql=parent

  label  Text sql=label
  active Bool sql=active
  leader Bool sql=leader

  synonyms [Text] Maybe sql=synonyms

  diagTree DiagSlideId Maybe sql=diagtree

  mailAddr Text Maybe sql=mailaddr
  mailPass Text Maybe sql=mailpass

  contacts [SubProgramContactId] sql=contacts
  services [SubProgramServiceId] sql=services

  checkPeriod Int Maybe sql=checkperiod
  validFor    Int Maybe sql=validfor

  defaultMake CarMakeId Maybe sql=defaultmake

  smsSender  Text sql=smssender
  smsContact Text sql=smscontact
  smsProgram Text sql=smsprogram

  eraGlonassParticipant Bool sql=eraglonassparticipant

  contractPrs [SubProgramContractPermissionId] sql=contractpermissions

  template Reference Maybe sql=template
  logo     Reference Maybe sql=logo

  help       Text Maybe sql=help
  dealerHelp Text Maybe sql=dealerhelp

  deriving Typeable Show

SubProgramService sql=SubProgramService
  sParent SubProgramId sql=parent

  -- TODO This is wrapped in @Maybe@ only because the client first
  -- creates an empty instance, then rendering a form where the type
  -- may be selected.
  sType ServiceTypeId Maybe sql=type

  maxCost     Text Maybe sql=maxcost
  maxDistance Int  Maybe sql=maxdistance
  maxPeriod   Int  Maybe sql=maxperiod
  maxCount    Int  Maybe sql=maxcount

  deriving Typeable Show

SubProgramContact sql=SubProgramContact
  cParent SubProgramId sql=parent
  name    Text Maybe   sql=name
  email   Text Maybe   sql=email
  phone   Text Maybe   sql=phone

  deriving Typeable Show

SubProgramContractPermission sql=SubProgramContractPermission
  fParent   SubProgramId sql=parent

  -- TODO This must be limited to @Contract@ field names only.
  --
  -- Wrapped in @Maybe@ just like the type field in @SubProgramService@.
  field     Text Maybe   sql=contractfield

  showTable Bool         sql=showtable
  showForm  Bool         sql=showform

  deriving Typeable Show
|]


-- | @SubProgram@ predefined IDs.
peugeotWarranty, citroenWarranty, cad2012, ramc, ford :: SubProgramId
peugeotWarranty = toSqlKey 3
citroenWarranty = toSqlKey 4
cad2012 = toSqlKey 9
ramc = toSqlKey 102
ford = toSqlKey 14
