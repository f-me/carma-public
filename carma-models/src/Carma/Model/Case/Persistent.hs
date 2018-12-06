{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.Case.Persistent where

import           Data.Text (Text)
import           Data.Time.Calendar (Day)
import           Data.Time.Clock (UTCTime)
import           Data.Typeable

import           Database.Persist.TH

import           Carma.Model.LegacyTypes

import           Carma.Model.Usermeta.Persistent (UsermetaId)
import           Carma.Model.CaseSource.Persistent (CaseSourceId)
import           Carma.Model.CaseStatus.Persistent (CaseStatusId)
import           Carma.Model.City.Persistent (CityId)
-- import           Carma.Model.Partner.Persistent (PartnerId)
import           Carma.Model.Program.Persistent (ProgramId)
import           Carma.Model.SubProgram.Persistent (SubProgramId)
import           Carma.Model.Transmission.Persistent (TransmissionId)
import           Carma.Model.Engine.Persistent (EngineId)
import           Carma.Model.CarClass.Persistent (CarClassId)
import           Carma.Model.CarGeneration.Persistent (CarGenerationId)
import           Carma.Model.CarMake.Persistent (CarMakeId)
import           Carma.Model.CarModel.Persistent (CarModelId)
import           Carma.Model.Contract.Persistent (ContractId)
import           Carma.Model.ContractCheckStatus.Persistent
                   (ContractCheckStatusId)

-- Diagnostics models
import           Carma.Model.Diagnostics.Cause.Persistent (CauseId)
import           Carma.Model.Diagnostics.Part.Persistent (PartId)
import           Carma.Model.Diagnostics.Suggestion.Persistent (SuggestionId)
import           Carma.Model.Diagnostics.System.Persistent (SystemId)
import           Carma.Model.Diagnostics.Wazzup.Persistent (WazzupId)


-- | Partially implemented @Case@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Case sql=casetbl
  callDate        UTCTime  Maybe sql=calldate
  vwcreatedate    UTCTime  Maybe sql=vwcreatedate
  callTaker       UsermetaId     sql=calltaker
  customerComment Text     Maybe sql=customercomment
  comment         WazzupId Maybe sql=comment

  -- Diagnostic fields
  diagnosis1 SystemId     Maybe sql=diagnosis1
  diagnosis2 PartId       Maybe sql=diagnosis2
  diagnosis3 CauseId      Maybe sql=diagnosis3
  diagnosis4 SuggestionId Maybe sql=diagnosis4

  -- Contacts fields
  contact_name Text Maybe sql=contact_name
  contact_phone1 Phone Maybe sql=contact_phone1
  contact_phone2 Phone Maybe sql=contact_phone2
  contact_phone3 Phone Maybe sql=contact_phone3
  contact_phone4 Phone Maybe sql=contact_phone4
  contact_email Text Maybe sql=contact_email
  contact_contactOwner Checkbox Maybe sql=contact_contactowner
  contact_ownerName Text Maybe sql=contact_ownername
  contact_ownerPhone1 Phone Maybe sql=contact_ownerphone1
  contact_ownerPhone2 Phone Maybe sql=contact_ownerphone2
  contact_ownerPhone3 Phone Maybe sql=contact_ownerphone3
  contact_ownerPhone4 Phone Maybe sql=contact_ownerphone4
  contact_ownerEmail Text Maybe sql=contact_owneremail

  program ProgramId sql=program
  subprogram SubProgramId Maybe sql=subprogram

  contractIdentifier Text Maybe sql=contractidentifier
  contract ContractId Maybe sql=contract

  -- Data about a car
  car_vin Text Maybe sql=car_vin
  car_make CarMakeId Maybe sql=car_make
  car_model CarModelId Maybe sql=car_model
  car_generation CarGenerationId Maybe sql=car_generation
  -- TODO car_seller PartnerId Maybe sql=car_seller
  car_plateNum Text Maybe sql=car_platenum
  car_makeYear Int Maybe sql=car_makeyear
  car_color Text Maybe sql=car_color
  car_buyDate Day Maybe sql=car_buydate
  car_firstSaleDate Day Maybe sql=car_firstsaledate
  -- TODO car_dealerTO PartnerId Maybe sql=car_dealerto
  car_mileage Int Maybe sql=car_mileage
  car_transmission TransmissionId Maybe sql=car_transmission
  car_engine EngineId Maybe sql=car_engine
  car_liters Text Maybe sql=car_liters
  car_class CarClassId Maybe sql=car_class

  vinChecked ContractCheckStatusId Maybe sql=vinchecked
  city CityId Maybe sql=city
  caseAddress_city CityId Maybe sql=caseaddress_city
  caseAddress_address PickerField Maybe sql=caseaddress_address
  caseAddress_comment Text Maybe sql=caseaddress_comment
  caseAddress_notRussia Checkbox Maybe sql=caseaddress_notrussia
  caseAddress_coords PickerField Maybe sql=caseaddress_coords
  caseAddress_map MapField Maybe sql=caseaddress_map
  temperature Text Maybe sql=temperature
  repair Day Maybe sql=repair
  accord Text Maybe sql=accord
  dealerCause Text Maybe sql=dealercause
  caseStatus CaseStatusId sql=casestatus
  psaExportNeeded Checkbox Maybe sql=psaexportneeded
  psaExported Checkbox Maybe sql=psaexported
  claim Text Maybe sql=claim

  -- It is "ephemeral" field, doesn't exists in the database
  -- services Reference sql=services

  files Reference Maybe sql=files
  source CaseSourceId sql=source
  acStart UTCTime Maybe sql=acstart
  isCreatedByEraGlonass Bool sql=iscreatedbyeraglonass

  deriving Typeable Show
|]
