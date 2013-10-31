
module Carma.Model.ServiceInfo where


import Data.Text (Text)
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Program (Program)

data ServiceInfo = ServiceInfo
  { program :: F (Ident Program) "program" "Программа"
  , service :: F Text            "service" "Услуга"
  , info    :: F Text            "info"    "Условия"
  } deriving Typeable

instance Model ServiceInfo where
  type TableName ServiceInfo = "ServiceInfo"
  modelInfo = mkModelInfo ServiceInfo
  modelView _ = modifyView defaultView [readonly program, readonly service]

