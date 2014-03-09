{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.Program where

import Data.Aeson as A (Value(Bool))
import Data.Text
import Data.Typeable
import Data.Vector

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.Types()
import Carma.Model.ProgramType hiding (ident)
import Carma.Model.Usermeta hiding (ident)
import Carma.Model.PgTypes()


data Program = Program
  { ident                 :: PK Int Program             "Программа"
  , label                 :: F Text            "label"  "Название"
  , client                :: F (Maybe Text)    "client" "Заказчик"
  , clientAddress         :: F (Maybe Text)    "clientAddress" "Адрес заказчика"
  , clientCode            :: F (Maybe Text)    "clientCode" "Код заказчика"
  , managers              :: F (Maybe (Vector (IdentI Usermeta)))
                             "managers"  "Менеджеры по программе"
  , pType                 :: F (Maybe (IdentI ProgramType))
                             "pType"  "Тип программы"
  } deriving Typeable


mkIdents [t|Program|]
 [ ("vtb24", 2)
 , ("genser", 32)
 , ("ramc", 64)
 , ("gm", 4)
 ]


instance Model Program where
  type TableName Program = "Program"
  idents = Carma.Model.Program.idents
  modelInfo = mkModelInfo Program ident
  modelView _ = modifyView defaultView
                [ setMeta "dictionaryType" "ComputedDict" managers
                , setMeta "dictionaryName" "programManagers" managers
                , setMeta "bounded" (A.Bool True) managers
                ]
