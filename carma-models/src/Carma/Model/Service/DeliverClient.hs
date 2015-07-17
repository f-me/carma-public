module Carma.Model.Service.DeliverClient where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.LegacyTypes

import Carma.Model.DeliveryType (DeliveryType)
import Carma.Model.Service      (Service)


data DeliverClient = DeliverClient
  { ident :: PK Int DeliverClient ""
  , deliveryType :: F (Maybe (IdentI DeliveryType)) "deliveryType" "Тип доставки"

  , deliverFrom_address :: F PickerField "deliverFrom_address" "Откуда везти"
  , deliverFrom_comment :: F (Maybe Text)"deliverFrom_comment" "Примечания"
  , deliverFrom_coords  :: F PickerField "deliverFrom_coords" "Координаты"
  , deliverFrom_map     :: F MapField    "deliverFrom_map" ""

  , deliverTo_address :: F PickerField "deliverTo_address" "Куда везти"
  , deliverTo_comment :: F (Maybe Text)"deliverTo_comment" "Примечания"
  , deliverTo_coords  :: F PickerField "deliverTo_coords" "Координаты"
  , deliverTo_map     :: F MapField    "deliverTo_map" ""
  }
  deriving Typeable


instance Model DeliverClient where
  type TableName DeliverClient = "deliverclienttbl"
  type Parent DeliverClient = Service
  parentInfo = ExParent modelInfo
  modelInfo = mkModelInfo DeliverClient ident
  modelView v = case parentView v :: Maybe (ModelView DeliverClient) of
    Nothing -> Nothing
    Just mv -> Just
      $ modifyView (mv {mv_title = "Доставка клиента к отремонтированному автомобилю"})
      $ mapWidget deliverFrom_address deliverFrom_coords deliverFrom_map
      ++ mapWidget deliverTo_address deliverTo_coords deliverTo_map
