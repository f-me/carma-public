
module Carma.Model.Service.Transportation where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.LegacyTypes
import Carma.Model.Service (Service)


data Transportation = Transportation
  { ident :: PK Int Transportation ""
  , fromToAddress_address :: F PickerField "fromToAddress_address" "Адрес куда/откуда"
  , fromToAddress_comment :: F Text        "fromToAddress_comment" "Примечания"
  , fromToAddress_coords  :: F PickerField "fromToAddress_coords" "Координаты"
  , fromToAddress_map     :: F MapField    "fromToAddress_map" ""
  }
  deriving Typeable


instance Model Transportation where
  type TableName Transportation = "transportationtbl"
  type Parent Transportation = Service
  modelInfo = mkModelInfo Transportation ident
  modelView _ = (defaultView :: ModelView Transportation)
    {mv_title = "Транспортировка"}
