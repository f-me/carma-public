{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.SubProgram.Type

where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.TH

import Carma.Model.Types (TInt)
import Carma.Model.LegacyTypes (Reference)
import Carma.Model.Program hiding (ident)


data SubProgram = SubProgram
  { ident       :: PK Int SubProgram
                   "Подпрограмма"
  , parent      :: F (IdentI Program)
                   "parent"
                   "Программа"
  , label       :: F Text
                   "label"
                   "Название"
  , active      :: F Bool
                   "active"
                   "Активна"
  , value       :: F Text
                   "value"
                   "Внутренняя метка"
  , mailAddr    :: F (Maybe Text)
                   "mailAddr"
                   "Mail для отправки писем"
  , mailPass    :: F (Maybe Text)
                   "mailPass"
                   "Пароль для отправки писем"
  , contacts    :: F (Maybe Text)
                   "contacts"
                   "Контактные лица"
  , services    :: F (Maybe Reference)
                   "services"
                   "Услуги, предоставляемые по программе"
  , checkPeriod :: F (Maybe TInt)
                   "checkPeriod"
                   "Межсервисный интервал по умолчанию"
  , validFor    :: F (Maybe TInt)
                   "validFor"
                   "Срок действия программы по умолчанию"
  , contract    :: F (Maybe Reference)
                   "contract"
                   "Шаблон договора"
  , logo        :: F (Maybe Reference)
                   "logo"
                   "Логотип"
  , help        :: F (Maybe Text)
                   "help"
                   "Справка"
  , dealerHelp  :: F (Maybe Text)
                   "dealerHelp"
                   "Справка для дилеров"
  } deriving Typeable


mkIdents [t|SubProgram|]
 [ ("vwMotor", 1)
 , ("vwCargo", 2)
 , ("peugeot", 3)
 , ("citroen", 4)
 ]
