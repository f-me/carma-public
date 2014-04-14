{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Carma.Model.SubProgram
    ( module Carma.Model.SubProgram.Type
    , module Carma.Model.SubProgram
    )

where

import Data.Aeson as A (Value(Bool, String))

import Data.Model
import Data.Model.TH
import Data.Model.View

import Carma.Model.SubProgram.Type
import Carma.Model.SubProgramContact            ()
import Carma.Model.SubProgramContractPermission ()
import Carma.Model.SubProgramService            ()


mkIdents [t|SubProgram|]
 [ ("peugeotWarranty", 3)
 , ("citroenWarranty", 4)
 , ("cad2012", 9)
 , ("ramc", 102)
 ]


instance Model SubProgram where
  type TableName SubProgram = "SubProgram"
  idents = Carma.Model.SubProgram.idents
  modelInfo = mkModelInfo SubProgram ident
  modelView = \case
    "" -> Just $ modifyView defaultView
                [ setMeta "regexp" "number" checkPeriod
                , setMeta "regexp" "number" validFor
                , setMeta "regexp" "email" mailAddr
                , required parent
                , required label
                , required value
                , infoText "subProgramValue" value
                , widget "text" checkPeriod
                , widget "text" validFor
                , setMeta "reference-label"
                  (A.String "Добавить контактное лицо") contacts
                , setMeta "reference-widget" "subprogram-contacts" contacts
                , setMeta "reference-label"
                  (A.String "Добавить услугу") services
                , setMeta "reference-widget" "subprogram-services" services
                , setMeta "reference-label"
                  (A.String "Добавить поле")
                  contractPrs
                , setMeta
                  "reference-widget" "subprogram-contract-fields"
                  contractPrs
                , textarea help
                , textarea dealerHelp
                , setMeta "widget" "inline-uploader" template
                , setMeta "reference-widget" "files" template
                , setMeta "widget" "inline-uploader" logo
                , setMeta "reference-widget" "files" logo
                , setMeta "single-uploader" (A.Bool True) logo
                ]
    _  -> Nothing
