
module Carma.Model.NewCaseField where

import Data.Text
import Data.Model
import Data.Typeable
import Data.Vector (Vector)

import Data.Model.View
import Carma.Model.Program (Program)


data NewCaseField = NewCaseField
  { field -- FIXME: Dictionary
    :: F Text                       "field"       "Внутреннее название поля"
  , program
    :: F (Ident Program)            "program"     "Название программы"
  , label
    :: F Text                       "label"       "Подпись к полю"
  , info
    :: F Text                       "info"        "Текст для всплывающей подсказки"
  , required
    :: F Bool                       "required"    "Обязательное поле"
  , r
    :: F Bool                       "r"           "Доступно для чтения"
  , w
    :: F Bool                       "w"           "Доступно для записи"
  }
  deriving Typeable


instance Model NewCaseField where
  type TableName NewCaseField = "NewCaseField"
  modelFields = getModelFields NewCaseField
  modelView _ = defaultView

