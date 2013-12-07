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
import Carma.Model.Usermeta hiding (ident)

data Program = Program
  { ident                 :: PK Int Program ""
  , label                 :: F Text            "label"  "Название"
  , client                :: F (Maybe Text)    "client" "Заказчик"
  , clientAddress         :: F (Maybe Text)    "clientAddress" "Адрес заказчика"
  , clientCode            :: F (Maybe Text)    "clientCode" "Код заказчика"
  , managers              :: F (Vector (IdentI Usermeta))
                             "managers"  "Менеджеры по программе"
  } deriving Typeable

mkIdents [t|Program|]
 [ ("vtb24", 2)
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
