module Carma.Model.CtrModel where

import Data.Text
import Data.Typeable
import Data.Model
import Data.Model.View
import Carma.Model.Types()
import Carma.Model.PgTypes()


data CtrModel = CtrModel
  {ident    :: PK Int CtrModel ""
  ,value    :: F Text "value" "Название модели"
  ,label    :: F Text "label" "Человекопонятное название модели"
  }
  deriving Typeable


instance Model CtrModel where
  type TableName CtrModel = "CtrModel"
  modelInfo = mkModelInfo CtrModel ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
