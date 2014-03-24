module Carma.Model.CtrScreen where

import Data.Text
import Data.Typeable
import Data.Model
import Data.Model.View
import Carma.Model.Types()
import Carma.Model.PgTypes()


data CtrScreen = CtrScreen
  {ident    :: PK Int CtrScreen ""
  ,value    :: F Text "value" "Название экрана"
  ,label    :: F Text "label" "Человекопонятное название экрана"
  }
  deriving Typeable


instance Model CtrScreen where
  type TableName CtrScreen = "CtrScreen"
  modelInfo = mkModelInfo CtrScreen ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
