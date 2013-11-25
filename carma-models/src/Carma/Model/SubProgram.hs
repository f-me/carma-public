module Carma.Model.SubProgram where

import Data.Time.Calendar (Day)
import Data.Text
import Data.Typeable
import Data.Vector

import Data.Model
import Data.Model.View

import Carma.Model.Types ()
import Carma.Model.LegacyTypes (Reference)
import Carma.Model.Program hiding (ident)
import Carma.Model.ServiceNames hiding (ident)

data SubProgram = SubProgram
  { ident        :: PK Int SubProgram
  , parent       :: F (IdentI Program)         "parent"    "Программа"
  , label        :: F Text                     "label"     "Название"
  , active       :: F Bool                     "active"    "Активна"
  , value        :: F Text                     "value"     "Внутренняя метка"
  , mailAddr     :: F (Maybe Text)             "mailAddr"  "Mail для отправки писем"
  , mailPass     :: F (Maybe Text)             "mailPass"  "Пароль для отправки писем"
  , contacts     :: F (Maybe Text)             "contacts"  "Контактные лица"
  , services     :: F (Vector (IdentI ServiceNames))
                    "services"  "Услуги, предоставляемые по программе"
  , checkPeriod  :: F (Maybe Int)
                    "checkPeriod"  "Межсервисный интервал по умолчанию"
  , validUntil   :: F (Maybe Int)
                    "validUntil"   "Срок действия программы по умолчанию"
  , contract     :: F (Maybe Reference)        "contract" "Шаблон договора"
  , logo         :: F (Maybe Reference)        "logo" "Логотип"
  , help         :: F (Maybe Text)             "help" "Справка"
  , dealerHelp   :: F (Maybe Text)             "dealerHelp" "Справка для дилеров"
  } deriving Typeable

instance Model SubProgram where
  type TableName SubProgram = "SubProgram"
  modelInfo = mkModelInfo SubProgram ident
  modelView _ = defaultView
