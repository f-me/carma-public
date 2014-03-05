
module Carma.Model.DefaultNewCaseField where

import Data.Text
import Data.Typeable
import Data.Model
import Data.Model.View
import Carma.Model.Types()
import Carma.Model.PgTypes()

data DefaultNewCaseField = DefaultNewCaseField
  {ident    :: PK Int DefaultNewCaseField ""
  ,field    :: F Text "field"    "Внутреннее название поля"
  ,label    :: F Text "label"    "Подпись к полю"
  ,info     :: F Text "info"     "Текст для всплывающей подсказки"
  ,required :: F Bool "required" "Обязательное поле"
  ,r        :: F Bool "r"        "Доступно для чтения"
  ,w        :: F Bool "w"        "Доступно для записи"
  }
  deriving Typeable


instance Model DefaultNewCaseField where
  type TableName DefaultNewCaseField = "DefaultNewCaseField"
  modelInfo = mkModelInfo DefaultNewCaseField ident
  modelView _ = modifyView defaultView [textarea info, readonly field]
