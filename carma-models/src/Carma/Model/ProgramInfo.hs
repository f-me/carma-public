
module Carma.Model.ProgramInfo where


import Data.Text (Text)
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Program (Program)

data ProgramInfo = ProgramInfo
  {ident   :: PK Int ProgramInfo
  ,program :: F (IdentI Program) "program" "Программа"
  ,info    :: F Text             "info"    "Условия"
  } deriving Typeable

instance Model ProgramInfo where
  type TableName ProgramInfo = "ProgramInfo"
  modelInfo = mkModelInfo ProgramInfo ident
  modelView _ = modifyView defaultView [readonly program, textarea info]

