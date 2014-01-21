
module Carma.Model.Service.Insurance where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.LegacyTypes
import Carma.Model.Service (Service)


data Insurance = Insurance
  { ident :: PK Int Insurance ""
  , requestType :: F (IdentT RequestType) "requestType" "Тип запроса"
  , whatToSay1  :: F Text                 "whatToSay1"  "Описание проблемы"
  , activity    :: F (IdentT Activity)    "activity"    "Тип действия"
  , commAddress_address :: F PickerField "commAddress_address" "Адрес выезда аваркома"
  , commAddress_comment :: F Text        "commAddress_comment" "Примечания"
  , commAddress_coords  :: F PickerField "commAddress_coords"  "Координаты"
  , commAddress_map     :: F MapField    "commAddress_map"     ""
  , commMilage          :: F Text        "commMilage" "Пробег аваркома за городом"
  }
  deriving Typeable

instance Model Insurance where
  type TableName Insurance = "insurancetbl"
  type Parent Insurance = Service
  modelInfo = mkModelInfo Insurance ident
  modelView _ = modifyView
    (defaultView :: ModelView Insurance)
      {mv_title = "Сбор справок для страховой компании"}
    $ mapWidget commAddress_address commAddress_coords commAddress_map
