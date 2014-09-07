{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.VinFormat
    ( VinFormat(..)
    , Carma.Model.VinFormat.idents, arc, arcFord
    , Sing(..)
    , ContractField
    , FormatFieldAccessor(..)
    , FormatFieldType(..)
    , vinFormatAccessors
    , ffaTitles
    , identModelName
    )

where

import Data.Text (Text, pack)
import Data.Typeable
import Data.Vector (Vector, toList)

import Data.Model
import Data.Model.Patch as Patch
import Data.Model.TH
import Data.Model.View

import Carma.Model.VinFormat.Meta

import qualified Carma.Model.Contract as Contract
import Carma.Model.Contract hiding (ident)

import Carma.Model.CarClass     (CarClass)
import Carma.Model.CarMake      (CarMake)
import Carma.Model.CarModel     (CarModel)
import Carma.Model.CheckType    (CheckType)
import Carma.Model.LegalForm    (LegalForm)
import Carma.Model.Partner      (Partner, partnerKey)
import Carma.Model.SubProgram   (SubProgram)
import Carma.Model.Transmission (Transmission)
import Carma.Model.Engine       (Engine)


-- Produce 'VinFormat' model from 'Contract'. All mentioned Contract
-- fields must be nullable (any field may be marked as optional by
-- users).
mkVinFormat  [ FF SName   Contract.name
             , FF SEmail  Contract.email
             , FF SVIN    Contract.vin
             , FF SRaw    Contract.cardNumber
             , FF SRaw    Contract.codeWord
             , FF SPhone  Contract.phone
             , FF SPlate  Contract.plateNum
             , FF SDate   Contract.validSince
             , FF SDate   Contract.validUntil
             , FF SNumber Contract.startMileage
             , FF SDict   Contract.make
             , FF SDict   Contract.model
             , FF SYear   Contract.makeYear
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


mkIdents [t|VinFormat|]
 [ ("arc", 1000)
 , ("arcFord", 2000)
 ]


instance Model VinFormat where
  type TableName VinFormat = "VinFormat"
  modelInfo = mkModelInfo VinFormat ident
  modelView = \case
    "" -> Just $ modifyView (defaultView :: ModelView VinFormat) $
                [ partnerKey lastCheckDealerDefault
                , partnerKey sellerDefault
                ]
    _  -> Nothing


-- | Export a list of titles from a field.
--
-- TODO Refactor this so that we can match on TitleParameter singleton
-- (requires type witness that TitleParameter (SFFT a) ~ SFFL v).
ffaTitles :: FormatFieldAccessor VinFormat -> Patch.Patch VinFormat -> [Text]
ffaTitles (FFAcc _ sTag _ _ _ tAcc) vf =
    let
        textProj "" = []
        textProj t  = [t]
        vecProj v = Prelude.filter (/= "") $ toList v
        proj = case sTag of
                 SRaw        -> textProj
                 SNumber     -> textProj
                 SVIN        -> textProj
                 SEmail      -> textProj
                 SPlate      -> textProj
                 SYear       -> textProj
                 SPhone      -> textProj
                 SName       -> vecProj
                 SDate       -> textProj
                 SDict       -> textProj
                 SDealer     -> vecProj
                 SSubprogram -> textProj
    in
      proj $ Patch.get' vf tAcc


-- | Extract target model name from an ident field (@Maybe (IdentI Model)@).
--
-- TODO Include TypeRep of the target model in FormatFieldAccessor
-- when SDict is speciifed.
identModelName :: forall m t n d. Typeable t => (m -> F t n d) -> Text
identModelName _ =
    pack $ show $
    head $ tail $ typeRepArgs $
    head $ typeRepArgs $ typeOf (undefined :: t)
