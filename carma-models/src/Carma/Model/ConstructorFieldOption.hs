
module Carma.Model.ConstructorFieldOption where

import Data.Text
import Data.Typeable
import Data.Model
import Data.Model.View
import Carma.Model.Types()
import Carma.Model.Program (Program)


data ConstructorFieldOption = ConstructorFieldOption
  {ident    :: PK Int ConstructorFieldOption ""
  ,model    :: F Text  "model"    "Модель к которой относится поле"
  ,screen   :: F Text  "screen"   "Экран"
  ,program  :: F (Maybe Text) -- FIXME: (Maybe (IdentI Program))
                       "program"  "Программа"
  ,ord      :: F Int   "ord"      "Порядок сортировки"
  ,field    :: F Text  "field"    "Внутреннее название поля"
  ,label    :: F Text  "label"    "Подпись к полю"
  ,info     :: F Text  "info"     "Текст для всплывающей подсказки"
  ,required :: F Bool  "required" "Обязательное поле"
  ,r        :: F Bool  "r"        "Доступно для чтения"
  ,w        :: F Bool  "w"        "Доступно для записи"
  }
  deriving Typeable


instance Model ConstructorFieldOption where
  type TableName ConstructorFieldOption = "ConstructorFieldOption"
  modelInfo = mkModelInfo ConstructorFieldOption ident
  modelView _ = modifyView defaultView
    [textarea info, readonly field, invisible ord]
