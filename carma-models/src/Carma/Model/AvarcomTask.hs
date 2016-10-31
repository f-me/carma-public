
module Carma.Model.AvarcomTask where

import Data.Text
import Data.Model
import Data.Typeable
import Data.Model.View
import Carma.Model.PgTypes ()


data AvarcomTask = AvarcomTask
  {ident    :: PK Int AvarcomTask ""
  ,label    :: F  Text "label"  "Наименование задачи"
  ,isAcitve :: F  Bool "isActive" "Активно"
  } deriving Typeable


instance Model AvarcomTask where
  type TableName AvarcomTask = "AvarcomTask"
  modelInfo = mkModelInfo AvarcomTask ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing

