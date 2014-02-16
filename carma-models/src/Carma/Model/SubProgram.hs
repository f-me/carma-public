{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Carma.Model.SubProgram
    ( SubProgram(..)
    )

where

import Data.Aeson as A (Value(Bool, String))

import Data.Model
import Data.Model.TH
import Data.Model.View

import Carma.Model.SubProgram.Type
import Carma.Model.SubProgramService hiding (ident)


mkIdents [t|SubProgram|]
 [ ("vwMotor", 1)
 , ("vwCargo", 2)
 , ("peugeot", 3)
 , ("citroen", 4)
 ]

instance Model SubProgram where
  type TableName SubProgram = "SubProgram"
  idents = Carma.Model.SubProgram.idents
  modelInfo = mkModelInfo SubProgram ident
  modelView _ = modifyView defaultView
                [ setMeta "regexp" "number" checkPeriod
                , setMeta "regexp" "number" validFor
                , setMeta "regexp" "email" mailAddr
                , widget "text" checkPeriod
                , widget "text" validFor
                , setMeta "reference-label"
                  (A.String "Добавить услугу") services
                , setMeta "adoptReferences" (A.Bool True) services
                , setMeta "model"
                  (A.String $
                   modelName (modelInfo :: ModelInfo SubProgramService))
                  services
                , textarea help
                , textarea dealerHelp
                , setMeta "widget" "inline-uploader" contract
                , setMeta "reference-widget" "files" contract
                , setMeta "widget" "inline-uploader" logo
                , setMeta "reference-widget" "files" logo
                , setMeta "single-uploader" (A.Bool True) logo
                ]
