{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.Program where

import Data.Aeson as A (Value(Bool))
import Data.Text
import Data.Typeable
import Data.Vector

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.ProgramType hiding (b2c, ident)
import Carma.Model.Usermeta hiding (ident)
import Carma.Model.PgTypes()


data Program = Program
  { ident                 :: PK Int Program             "Программа"
  , label                 :: F Text            "label"  "Название"
  , client                :: F (Maybe Text)    "client" "Заказчик"
  , clientAddress         :: F (Maybe Text)    "clientAddress" "Адрес заказчика"
  , clientCode            :: F (Maybe Text)    "clientCode" "Код заказчика"
  , fdds                  :: F (Maybe Text)    "fdds" "FDDS-код"
  , managers              :: F (Maybe (Vector (IdentI Usermeta)))
                             "managers"  "Менеджеры по программе"
  , pType                 :: F (Maybe (IdentI ProgramType))
                             "pType"  "Тип программы"
  , help                  :: F (Maybe Text)
                             "help" "Справка"
  } deriving Typeable


mkIdents [t|Program|]
 [ ("arc", 30)
 , ("atlant", 11)
 , ("avilon", 18)
 , ("b2c", 15)
 , ("citroen", 67)
 , ("euro", 9)
 , ("ford", 6)
 , ("genser", 32)
 , ("gm", 4)
 , ("nz", 17)
 , ("mapfre", 7)
 , ("peugeot", 1)
 , ("ramc", 64)
 , ("ruslan", 16)
 , ("unicredit", 47)
 , ("vnukovo", 19)
 , ("vtb24", 2)
 , ("vw", 3)
 , ("alarm", 5)
 ]


instance Model Program where
  type TableName Program = "Program"
  idents = Carma.Model.Program.idents
  modelInfo = mkModelInfo Program ident
  modelView = \case
    "" -> Just $ modifyView defaultView
                [ setMeta "dictionaryType" "ComputedDict" managers
                , setMeta "dictionaryName" "programManagers" managers
                , setMeta "bounded" (A.Bool True) managers
                , textarea help
                , infoText "programHelp" help
                , required Carma.Model.Program.label
                ]
    _  -> Nothing
