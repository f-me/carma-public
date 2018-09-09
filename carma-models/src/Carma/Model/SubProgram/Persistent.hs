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


-- | Partially implemented @SubProgram@ persistent model.
mkPersist sqlSettings [persistLowerCase|
SubProgram sql=SubProgram
  parent ProgramId sql=parent

  label  Text sql=label
  active Bool sql=active
  leader Bool sql=leader

  synonyms Text Vector Maybe sql=synonyms

  diagTree DiagSlideId Maybe sql=diagtree

  mailAddr Text Maybe sql=mailaddr
  mailPass Text Maybe sql=mailpass

  contacts SubProgramContactId Vector sql=contacts
  -- TODO services SubProgramServiceId Vector sql=services

  checkPeriod Int Maybe sql=checkperiod
  validFor    Int Maybe sql=validfor

  defaultMake CarMakeId Maybe sql=defaultmake

  smsSender  Text sql=smssender
  smsContact Text sql=smscontact
  smsProgram Text sql=smsprogram

  eraGlonassParticipant Bool sql=eraglonassparticipant

  contractPrs SubProgramContractPermissionId Vector sql=contractpermissions

  template Reference Maybe sql=template
  logo     Reference Maybe sql=logo

  help       Text Maybe sql=help
  dealerHelp Text Maybe sql=dealerhelp

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
