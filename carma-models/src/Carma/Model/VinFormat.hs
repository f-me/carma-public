{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.VinFormat
    ( VinFormat(..)
    , ContractField(..), FF(..), FormatFieldType(..)
    , VFAccessor(..), ParamAcc(..)
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
import Carma.Model.Transmission (Transmission)
import Carma.Model.Engine       (Engine)


mkVinFormat  [ FF Name   Contract.name
             , FF Raw    Contract.email
             , FF Raw    Contract.vin
             , FF Raw    Contract.cardNumber
             , FF Raw    Contract.codeWord
             , FF Phone  Contract.phone
             , FF Raw    Contract.plateNum
             , FF Date   Contract.validSince
             , FF Date   Contract.validUntil
             , FF Number Contract.startMileage
             , FF Dict   Contract.make
             , FF Dict   Contract.model
             , FF Number Contract.makeYear
             , FF Dict   Contract.carClass
             , FF Raw    Contract.color
             , FF Dict   Contract.transmission
             , FF Raw    Contract.engineVolume
             , FF Dict   Contract.engineType
             , FF Date   Contract.buyDate
             , FF Dealer Contract.seller
             , FF Dealer Contract.lastCheckDealer
             , FF Number Contract.lastCheckMileage
             , FF Date   Contract.lastCheckDate
             , FF Number Contract.checkPeriod
             , FF Dict   Contract.checkType
             , FF Raw    Contract.orderNumber
             , FF Name   Contract.managerName
             , FF Raw    Contract.comment
             , FF Dict   Contract.legalForm
             , FF Subprogram Contract.subprogram
             ]

instance Model VinFormat where
  type TableName VinFormat = "VinFormat"
  modelInfo = mkModelInfo VinFormat ident
  modelView _ = modifyView defaultView
                [ setMeta "dictionaryParent" "makeDefault" modelDefault
                ]
