module Carma.Model.SubProgram where

import Data.Text
import Data.Typeable
import Data.Model
import Data.Model.View

import Carma.Model.Program hiding (ident)
import Carma.Model.Service hiding (ident)

data SubProgram = SubProgram
  { ident        :: PK Int SubProgram
  , parent       :: F (IdentI Program)   "parent"  "Программа"
  , mailAddr     :: F Text               "mailAddr"  "Mail для отправки писем"
  , mailPass     :: F Text               "mailPass"  "Пароль для отправки писем"
  , services     :: F ([IdentI Service]) "services"  "Услуги, предоставляемые по программе"
  , serviceEvery :: F Int "serviceEvery" "Межсервисный интервал по умолчанию"
--  , validThru   :: F 
  , contract     :: F Text               "contract" "Шаблон договора"
  , logo         :: F Text               "logo" "Логотип"
  , help         :: F Text               "help" "Справка"
  , dealerHelp   :: F Text               "dealerHelp" "Справка для дилеров"
  } deriving Typeable

instance Model SubProgram where
  type TableName SubProgram = "subprogramtbl"
  modelInfo = mkModelInfo SubProgram ident
  modelView _ = defaultView
