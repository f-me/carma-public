module Carma.Model.SubProgram where

import Data.Time.Calendar (Day)
import Data.Text
import Data.Typeable
import Data.Vector

import Data.Model
import Data.Model.View

import Carma.Model.Types ()
import Carma.Model.Program hiding (ident)
import Carma.Model.Service hiding (ident)

data SubProgram = SubProgram
  { ident        :: PK Int SubProgram
  , parent       :: F (IdentI Program) "parent"    "Программа"
  , mailAddr     :: F Text             "mailAddr"  "Mail для отправки писем"
  , mailPass     :: F Text             "mailPass"  "Пароль для отправки писем"
  , contacts     :: F Text             "contacts"  "Контактные лица"
  , services     :: F (Vector (IdentI Service))
                    "services"  "Услуги, предоставляемые по программе"
  , checkPeriod  :: F Int
                    "checkPeriod"  "Межсервисный интервал по умолчанию"
  , validUntil   :: F Day
                    "validUntil"   "Срок действия программы по умолчанию"
  , contract     :: F Reference        "contract" "Шаблон договора"
  , logo         :: F Reference        "logo" "Логотип"
  , help         :: F Text             "help" "Справка"
  , dealerHelp   :: F Text             "dealerHelp" "Справка для дилеров"
  } deriving Typeable

instance Model SubProgram where
  type TableName SubProgram = "SubProgram"
  modelInfo = mkModelInfo SubProgram ident
  modelView _ = defaultView
