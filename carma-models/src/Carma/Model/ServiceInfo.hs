
module Carma.Model.ServiceInfo where


import Data.Text (Text)
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Program (Program)

data ServiceInfo = ServiceInfo
  {ident   :: PK Int ServiceInfo
  ,program :: F (IdentI Program) "program" "Программа"
  ,service :: F Text             "service" "Услуга"
  ,info    :: F Text             "info"    "Условия"
  } deriving Typeable

instance Model ServiceInfo where
  type TableName ServiceInfo = "ServiceInfo"
  modelInfo = mkModelInfo ServiceInfo ident
  modelView _ = modifyView defaultView
    [readonly program, readonly service, textarea info]

