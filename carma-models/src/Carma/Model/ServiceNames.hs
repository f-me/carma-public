
module Carma.Model.ServiceNames where


import Data.Text (Text)
import Data.Typeable

import Carma.Model.Types()
import Carma.Model.PgTypes()

import Data.Model
import Data.Model.View

data ServiceNames = ServiceNames
  {ident   :: PK Int ServiceNames ""
  ,value   :: F Text "value" "Внутреннее название услуги"
  ,label   :: F Text "label" "Название услуги"
  ,icon    :: F Text "icon"  "Иконка"
  } deriving Typeable

instance Model ServiceNames where
  type TableName ServiceNames = "ServiceNames"
  modelInfo = mkModelInfo ServiceNames ident
  modelView _ = modifyView defaultView [readonly value]

