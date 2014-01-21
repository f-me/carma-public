{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.VinFormat
    ( VinFormat(..)
    , ContractField(..), FF(..)
    , VFAccessor(..)
    , vinFormatAccessors
    )

where

import Data.Text

import Data.Model
import Data.Model.View
import Data.Vector

import Carma.Model.Types (TInt)

import Carma.Model.VinFormat.Meta

import qualified Carma.Model.Contract as Contract
import Carma.Model.Contract hiding (ident)

import Carma.Model.CarClass     (CarClass)
import Carma.Model.CarMake      (CarMake)
import Carma.Model.CarModel     (CarModel)
import Carma.Model.CheckType    (CheckType)
import Carma.Model.Colors       (Colors)
import Carma.Model.LegalForm    (LegalForm)
import Carma.Model.Partner      (Partner)
import Carma.Model.SubProgram   (SubProgram)
import Carma.Model.Transmission (Transmission)
import Carma.Model.Engine       (Engine)


mkVinFormat  [ FF SName   Contract.name
             , FF SRaw    Contract.email
             , FF SRaw    Contract.vin
             , FF SRaw    Contract.cardNumber
             , FF SRaw    Contract.codeWord
             , FF SPhone  Contract.phone
             , FF SRaw    Contract.plateNum
             , FF SDate   Contract.validSince
             , FF SDate   Contract.validUntil
             , FF SNumber Contract.startMileage
             , FF SDict   Contract.make
             , FF SDict   Contract.model
             , FF SNumber Contract.makeYear
             , FF SDict   Contract.carClass
             , FF SRaw    Contract.color
             , FF SDict   Contract.transmission
             , FF SRaw    Contract.engineVolume
             , FF SDict   Contract.engineType
             , FF SDate   Contract.buyDate
             , FF SDealer Contract.seller
             , FF SDealer Contract.lastCheckDealer
             , FF SNumber Contract.lastCheckMileage
             , FF SDate   Contract.lastCheckDate
             , FF SNumber Contract.checkPeriod
             , FF SDict   Contract.checkType
             , FF SRaw    Contract.orderNumber
             , FF SName   Contract.managerName
             , FF SRaw    Contract.comment
             , FF SDict   Contract.legalForm
             , FF SSubprogram Contract.subprogram
             ]

instance Model VinFormat where
  type TableName VinFormat = "VinFormat"
  modelInfo = mkModelInfo VinFormat ident
  modelView _ = defaultView
