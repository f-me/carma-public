module Carma.Model.CallType where


import Data.Text (Text)
import Data.Typeable

import Carma.Model.Types()
import Carma.Model.PgTypes()

import Data.Model
import Data.Model.View

data CallType = CallType
  {ident   :: PK Int CallType ""
  ,label   :: F Text "label" "Название цвета"
  } deriving Typeable

instance Model CallType where
  type TableName CallType = "CallType"
  modelInfo = mkModelInfo CallType ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
