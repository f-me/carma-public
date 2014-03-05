
module Carma.Model.Colors where


import Data.Text (Text)
import Data.Typeable

import Carma.Model.Types()
import Carma.Model.PgTypes()

import Data.Model
import Data.Model.View

data Colors = Colors
  {ident   :: PK Int Colors ""
  ,value   :: F Text "value" "Внутреннее название цвета"
  ,label   :: F Text "label" "Название цвета"
  } deriving Typeable

instance Model Colors where
  type TableName Colors = "Colors"
  modelInfo = mkModelInfo Colors ident
  modelView _ = modifyView defaultView [readonly value]

