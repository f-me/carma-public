module Carma.Model.ServiceInfo where


import Data.Text (Text)
import Data.Typeable

import Carma.Model.Types()

import Data.Model
import Data.Model.View

import Carma.Model.Program (Program)
import Carma.Model.ServiceNames (ServiceNames)

data ServiceInfo = ServiceInfo
  {ident   :: PK Int ServiceInfo ""
  ,program :: F (IdentI Program)   "program" "Программа"
  ,service :: F (IdentI ServiceNames) "service" "Услуга"
  ,info    :: F Text                  "info"    "Условия"
  } deriving Typeable

instance Model ServiceInfo where
  type TableName ServiceInfo = "ServiceInfo"
  modelInfo = mkModelInfo ServiceInfo ident
  modelView = \case
    "" -> Just $ modifyView defaultView
      [readonly program, readonly service, textarea info]
    _  -> Nothing
