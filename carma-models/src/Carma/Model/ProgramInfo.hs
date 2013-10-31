
module Carma.Model.ProgramInfo where


import Data.Text (Text)
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Program (Program)

data ProgramInfo = ProgramInfo
  { program :: F (Ident Program) "program" "Программа"
  , info    :: F Text            "info"    "Условия"
  } deriving Typeable

instance Model ProgramInfo where
  type TableName ProgramInfo = "ProgramInfo"
  modelInfo = mkModelInfo ProgramInfo
  modelView _ = modifyView defaultView [readonly program]

