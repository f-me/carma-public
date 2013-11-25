
module Carma.Model.Service.Tickets where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.LegacyTypes
import Carma.Model.Service (Service)

data Tickets = Tickets
  { ident :: PK Int Tickets
  , ticketsFrom_address :: F PickerField "ticketsFrom_address" "Откуда"
  , ticketsFrom_comment :: F Text        "ticketsFrom_comment" "Примечания"
  , ticketsFrom_coords  :: F PickerField "ticketsFrom_coords" "Координаты"
  , ticketsFrom_map     :: F MapField    "ticketsFrom_map" ""

  , ticketsTo_address   :: F PickerField "ticketsTo_address" "Куда"
  , ticketsTo_comment   :: F Text        "ticketsTo_comment" "Примечания"
  , ticketsTo_coords    :: F PickerField "ticketsTo_coords" "Координаты"
  , ticketsTo_map       :: F MapField    "ticketsTo_map" ""

  , deliveryType        :: F (IdentT DeliveryType) "deliveryType" "Тип доставки"
  }
  deriving Typeable


instance Model Tickets where
  type TableName Tickets = "ticketstbl"
  type Parent Tickets = Service
  modelInfo = mkModelInfo Tickets ident
  modelView _ = (defaultView :: ModelView Tickets)
    {mv_title = "Заказ билетов"}
