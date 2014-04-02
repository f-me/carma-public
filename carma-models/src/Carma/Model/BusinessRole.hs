module Carma.Model.BusinessRole where

import Data.Text (Text)
import Data.Typeable
import Data.Model
import Data.Model.View
import Carma.Model.Types()
import Carma.Model.PgTypes()

data BusinessRole = BusinessRole
  {ident  :: PK Int BusinessRole ""
  ,label  :: F Text "label"  "Название бизнес-роли"
  } deriving Typeable

instance Model BusinessRole where
  type TableName BusinessRole = "BusinessRole"
  modelInfo = mkModelInfo BusinessRole ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
