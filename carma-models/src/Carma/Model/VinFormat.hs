{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.VinFormat
    ( VinFormat(..)
    , Sing(..)
    , ContractField(..)
    , FormatFieldAccessor(..)
    , FormatFieldType(..)
    , vinFormatAccessors
    , ffaTitles
    )

where

import Data.Text
import Data.Vector

import Data.Model
import Data.Model.Patch as Patch
import Data.Model.View

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


-- | Export a list of titles from a field.
--
-- TODO Refactor this so that we can match on TitleParameter singleton
-- (requires witness that TitleParameter (SFFT a) ~ SFFL v).
ffaTitles :: FormatFieldAccessor VinFormat -> Patch.Patch VinFormat -> [Text]
ffaTitles (FFAcc _ sTag _ _ _ tAcc) vf =
    let
        textProj "" = []
        textProj t  = [t]
        vecProj v = Prelude.filter (/= "") $ toList v
        proj = case sTag of
                 SRaw        -> textProj
                 SNumber     -> textProj
                 SPhone      -> textProj
                 SName       -> vecProj
                 SDate       -> textProj
                 SDict       -> textProj
                 SDealer     -> vecProj
                 SSubprogram -> textProj
    in
      proj $ Patch.get' vf tAcc
