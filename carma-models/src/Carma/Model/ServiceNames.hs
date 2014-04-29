module Carma.Model.ServiceNames where


import Data.Text (Text)
import Data.Typeable

import Carma.Model.Types (TInt)
import Carma.Model.PgTypes()

import Data.Model
import Data.Model.View

data ServiceNames = ServiceNames
  {ident   :: PK Int ServiceNames ""
  ,value   :: F Text "value" "Внутреннее название услуги"
  ,label   :: F Text "label" "Название услуги"
  ,icon    :: F Text "icon"  "Иконка"
  ,fdds    :: F (Maybe Text) "fdds"  "FDDS-код"
  } deriving Typeable

instance Model ServiceNames where
  type TableName ServiceNames = "ServiceNames"
  modelInfo = mkModelInfo ServiceNames ident
  modelView = \case
    "" -> Just $ modifyView defaultView [readonly value]
    _  -> Nothing
