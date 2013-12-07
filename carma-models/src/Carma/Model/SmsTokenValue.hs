module Carma.Model.SmsTokenValue where

import Data.Text
import Data.Typeable
import Data.Model
import Data.Model.View

import Carma.Model.Types()
import Carma.Model.Program (Program)
import Carma.Model.SubProgram (SubProgram)
import Carma.Model.SmsTokenName (SmsTokenName)

data SmsTokenValue = SmsTokenValue
  { ident      :: PK Int SmsTokenValue    "Значения переменных шаблонов СМС"
  , token      :: F (IdentI SmsTokenName) "token"       "Переменная"
  , program    :: F (IdentI Program)      "program"     "Программа"
  , subProgram :: F (IdentI SubProgram)   "sub_program" "Подпрограмма"
  , value      :: F Text                  "value"       "Значение переменной"
  }
  deriving Typeable

instance Model SmsTokenValue where
  type TableName SmsTokenValue = "SmsTokenValue"
  modelInfo = mkModelInfo SmsTokenValue ident
  modelView _ = defaultView
