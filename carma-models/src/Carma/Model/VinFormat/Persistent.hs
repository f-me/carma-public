{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}

module Carma.Model.VinFormat.Persistent where

import           Data.Text                            (Text)
import           Data.Time.Calendar                   (Day)
import           Data.Typeable

import           Database.Persist.Sql                 (toSqlKey)
import           Database.Persist.TH

import           Carma.Model.CarClass.Persistent      (CarClassId)
import           Carma.Model.CarGeneration.Persistent (CarGenerationId)
import           Carma.Model.CarMake.Persistent       (CarMakeId)
import           Carma.Model.CarModel.Persistent      (CarModelId)
import           Carma.Model.CheckType.Persistent     (CheckTypeId)
import           Carma.Model.Engine.Persistent        (EngineId)
import           Carma.Model.LegalForm.Persistent     (LegalFormId)
import           Carma.Model.Partner.Persistent       (PartnerId)
import           Carma.Model.SubProgram.Persistent    (SubProgramId)
import           Carma.Model.Transmission.Persistent  (TransmissionId)


-- | @VinFormat@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
VinFormat sql=VinFormat
  label Text sql=label

  nameLoad     Bool         sql=nameload
  nameTitles   [Text]       sql=nametitles
  nameDefault  Text   Maybe sql=namedefault
  nameRequired Bool         sql=namerequired

  emailLoad     Bool        sql=emailload
  emailTitle    Text        sql=emailtitle
  emailDefault  Text  Maybe sql=emaildefault
  emailRequired Bool        sql=emailrequired

  vinLoad       Bool        sql=vinload
  vinTitle      Text        sql=vintitle
  vinDefault    Text  Maybe sql=vindefault
  vinRequired   Bool        sql=vinrequired

  cardNumberLoad     Bool        sql=cardnumberload
  cardNumberTitle    Text        sql=cardnumbertitle
  cardNumberDefault  Text  Maybe sql=cardnumberdefault
  cardNumberRequired Bool        sql=cardnumberrequired

  codeWordLoad       Bool        sql=codewordload
  codeWordTitle      Text        sql=codewordtitle
  codeWordDefault    Text  Maybe sql=codeworddefault
  codeWordRequired   Bool        sql=codewordrequired

  phoneLoad        Bool        sql=phoneload
  phoneTitle       Text        sql=phonetitle
  phoneDefault     Text  Maybe sql=phonedefault
  phoneRequired    Bool        sql=phonerequired

  plateNumLoad     Bool        sql=platenumload
  plateNumTitle    Text        sql=platenumtitle
  plateNumDefault  Text  Maybe sql=platenumdefault
  plateNumRequired Bool        sql=platenumrequired

  validSinceLoad     Bool        sql=validsinceload
  validSinceTitle    Text        sql=validsincetitle
  validSinceDefault  Day   Maybe sql=validsincedefault
  validSinceRequired Bool        sql=validsincerequired

  validUntilLoad     Bool        sql=validuntilload
  validUntilTitle    Text        sql=validuntiltitle
  validUntilDefault  Day   Maybe sql=validuntildefault
  validUntilRequired Bool        sql=validuntilrequired

  startMileageLoad     Bool        sql=startmileageload
  startMileageTitle    Text        sql=startmileagetitle
  startMileageDefault  Int   Maybe sql=startmileagedefault
  startMileageRequired Bool        sql=startmileagerequired

  makeLoad     Bool            sql=makeload
  makeTitle    Text            sql=maketitle
  makeDefault  CarMakeId Maybe sql=makedefault
  makeRequired Bool            sql=makerequired

  modelLoad     Bool             sql=modelload
  modelTitle    Text             sql=modeltitle
  modelDefault  CarModelId Maybe sql=modeldefault
  modelRequired Bool             sql=modelrequired

  makeYearLoad     Bool        sql=makeyearload
  makeYearTitle    Text        sql=makeyeartitle
  makeYearDefault  Int   Maybe sql=makeyeardefault
  makeYearRequired Bool        sql=makeyearrequired

  carClassLoad     Bool             sql=carclassload
  carClassTitle    Text             sql=carclasstitle
  carClassDefault  CarClassId Maybe sql=carclassdefault
  carClassRequired Bool             sql=carclassrequired

  colorLoad     Bool        sql=colorload
  colorTitle    Text        sql=colortitle
  colorDefault  Text  Maybe sql=colordefault
  colorRequired Bool        sql=colorrequired

  transmissionLoad     Bool                 sql=transmissionload
  transmissionTitle    Text                 sql=transmissiontitle
  transmissionDefault  TransmissionId Maybe sql=transmissiondefault
  transmissionRequired Bool                 sql=transmissionrequired

  engineVolumeLoad     Bool        sql=enginevolumeload
  engineVolumeTitle    Text        sql=enginevolumetitle
  engineVolumeDefault  Text  Maybe sql=enginevolumedefault
  engineVolumeRequired Bool        sql=enginevolumerequired

  engineTypeLoad     Bool           sql=enginetypeload
  engineTypeTitle    Text           sql=enginetypetitle
  engineTypeDefault  EngineId Maybe sql=enginetypedefault
  engineTypeRequired Bool           sql=enginetyperequired

  buyDateLoad     Bool        sql=buydateload
  buyDateTitle    Text        sql=buydatetitle
  buyDateDefault  Day   Maybe sql=buydatedefault
  buyDateRequired Bool        sql=buydaterequired

  sellerLoad     Bool            sql=sellerload
  sellerTitles   [Text]          sql=sellertitles
  sellerDefault  PartnerId Maybe sql=sellerdefault
  sellerRequired Bool            sql=sellerrequired

  lastCheckDealerLoad     Bool            sql=lastcheckdealerload
  lastCheckDealerTitles   [Text]          sql=lastcheckdealertitles
  lastCheckDealerDefault  PartnerId Maybe sql=lastcheckdealerdefault
  lastCheckDealerRequired Bool            sql=lastcheckdealerrequired

  checkPeriodLoad     Bool        sql=checkperiodload
  checkPeriodTitle    Text        sql=checkperiodtitle
  checkPeriodDefault  Int  Maybe  sql=checkperioddefault
  checkPeriodRequired Bool        sql=checkperiodrequired

  checkTypeLoad     Bool              sql=checktypeload
  checkTypeTitle    Text              sql=checktypetitle
  checkTypeDefault  CheckTypeId Maybe sql=checktypedefault
  checkTypeRequired Bool              sql=checktyperequired

  orderNumberLoad     Bool        sql=ordernumberload
  orderNumberTitle    Text        sql=ordernumbertitle
  orderNumberDefault  Text  Maybe sql=ordernumberdefault
  orderNumberRequired Bool        sql=ordernumberrequired

  managerNameLoad     Bool         sql=managernameload
  managerNameTitles   [Text]       sql=managernametitles
  managerNameDefault  Text   Maybe sql=managernamedefault
  managerNameRequired Bool         sql=managernamerequired

  commentLoad     Bool        sql=commentload
  commentTitle    Text        sql=commenttitle
  commentDefault  Text  Maybe sql=commentdefault
  commentRequired Bool        sql=commentrequired

  subProgramLoad     Bool               sql=subprogramload
  subProgramTitle    Text               sql=subprogramtitle
  subProgramDefault  SubProgramId Maybe sql=subprogramdefault
  subProgramRequired Bool               sql=subprogramrequired

  legalFormLoad     Bool              sql=legalformload
  legalFormTitle    Text              sql=legalformtitle
  legalFormDefault  LegalFormId Maybe sql=legalformdefault
  legalFormRequired Bool              sql=legalformrequired

  generationLoad     Bool                  sql=generationload
  generationTitle    Text                  sql=generationtitle
  generationDefault  CarGenerationId Maybe sql=generationdefault
  generationRequired Bool                  sql=generationrequired

  firstSaleDateLoad     Bool        sql=firstsaledateload
  firstSaleDateTitle    Text        sql=firstsaledatetitle
  firstSaleDateDefault  Day   Maybe sql=firstsaledatedefault
  firstSaleDateRequired Bool        sql=firstsaledaterequired

  deriving Typeable Show
|]


-- | @VinFormat@ predefined IDs.
arc, arcFord :: VinFormatId
arc = toSqlKey 1000
arcFord = toSqlKey 2000
