{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

-- | @Contract@ persistent model.
module Carma.Model.Contract.Persistent where

import           Data.Typeable
import           Data.Time.Clock (UTCTime)
import           Data.Time.Calendar (Day)
import           Data.Text (Text)
import           Data.Aeson (Value)

import           Database.Persist.TH

import           Database.Persist.Postgresql.JSON ()
                 -- ^ @PersistFieldSql Value@ instance

import           Carma.Model.Types (Price)
import           Carma.Model.CarMake.Persistent (CarMakeId)
import           Carma.Model.CarModel.Persistent (CarModelId)
import           Carma.Model.CarGeneration.Persistent (CarGenerationId)
import           Carma.Model.CarClass.Persistent (CarClassId)
import           Carma.Model.Transmission.Persistent (TransmissionId)
import           Carma.Model.Engine.Persistent (EngineId)
import           Carma.Model.Partner.Persistent (PartnerId)
import           Carma.Model.ContractRegistrationReason.Persistent
                   (ContractRegistrationReasonId)
import           Carma.Model.CheckType.Persistent (CheckTypeId)
import           Carma.Model.SubProgram.Persistent (SubProgramId)
import           Carma.Model.LegalForm.Persistent (LegalFormId)
import           Carma.Model.Usermeta.Persistent (UsermetaId)


type PriceInOrder = Price 10 2


-- | @Contract@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Contract json sql=Contract
  ctime UTCTime sql=ctime
  isActive Bool sql=isactive
  name Text Maybe sql=name
  email Text Maybe sql=email
  fromArc Bool sql=fromarc
  sourceFile Text sql=sourcefile
  vin Text Maybe sql=vin
  cardNumber Text Maybe sql=cardnumber
  codeWord Text Maybe sql=codeword
  phone Text Maybe sql=phone
  plateNum Text Maybe sql=platenum
  validSince Day Maybe sql=validsince
  validUntil Day Maybe sql=validuntil
  startMileage Int Maybe sql=startmileage
  make CarMakeId Maybe sql=make
  model CarModelId Maybe sql=model
  generation CarGenerationId Maybe sql=generation
  makeYear Int Maybe sql=makeyear
  carClass CarClassId Maybe sql=carclass
  color Text Maybe sql=color
  transmission TransmissionId Maybe sql=transmission
  engineVolume Text Maybe sql=enginevolume
  engineType EngineId Maybe sql=enginetype
  buyDate Day Maybe sql=buydate
  firstSaleDate Day Maybe sql=firstsaledate
  seller PartnerId Maybe sql=seller
  registrationReason ContractRegistrationReasonId Maybe sql=registrationreason
  priceInOrder PriceInOrder Maybe sql=priceinorder
  lastCheckDealer PartnerId Maybe sql=lastcheckdealer
  checkPeriod Int Maybe sql=checkperiod
  checkType CheckTypeId Maybe sql=checktype
  orderNumber Text Maybe sql=ordernumber
  managerName Text Maybe sql=managername
  comment Text Maybe sql=comment
  subprogram SubProgramId Maybe sql=subprogram
  legalForm LegalFormId Maybe sql=legalform
  committer UsermetaId sql=committer
  extra Value Maybe sql=extra
  dixi Bool sql=dixi

  deriving Typeable Show
|]
