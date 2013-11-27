
module Carma.Model.Service.SoberDriver where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.LegacyTypes
import Carma.Model.Service (Service)


data SoberDriver = SoberDriver
  { ident :: PK Int SoberDriver
  , fromAddress_address :: F PickerField "fromAddress_address" "Откуда везти"
  , fromAddress_comment :: F Text        "fromAddress_comment" "Примечания"
  , fromAddress_coords  :: F PickerField "fromAddress_coords" "Координаты"
  , fromAddress_map     :: F MapField    "fromAddress_map" ""

  , toAddress_address :: F PickerField "toAddress_address" "Куда везти"
  , toAddress_comment :: F Text        "toAddress_comment" "Примечания"
  , toAddress_coords  :: F PickerField "toAddress_coords" "Координаты"
  , toAddress_map     :: F MapField    "toAddress_map" ""
  }
  deriving Typeable


instance Model SoberDriver where
  type TableName SoberDriver = "sobertbl"
  type Parent SoberDriver = Service
  modelInfo = mkModelInfo SoberDriver ident
  modelView _ = (defaultView :: ModelView SoberDriver)
    {mv_title = "Трезвый водитель"}
