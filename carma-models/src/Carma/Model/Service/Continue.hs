
module Carma.Model.Service.Continue where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.LegacyTypes
import Carma.Model.Service (Service)


data Continue = Continue
  { ident :: PK Int Continue ""
  , deliveryType :: F (IdentT DeliveryType) "deliveryType" "Тип доставки"

  , deliverFrom_address :: F PickerField "deliverFrom_address" "Откуда везти"
  , deliverFrom_comment :: F Text        "deliverFrom_comment" "Примечания"
  , deliverFrom_coords  :: F PickerField "deliverFrom_coords" "Координаты"
  , deliverFrom_map     :: F MapField    "deliverFrom_map" ""

  , deliverTo_address :: F PickerField "deliverTo_address" "Куда везти"
  , deliverTo_comment :: F Text        "deliverTo_comment" "Примечания"
  , deliverTo_coords  :: F PickerField "deliverTo_coords" "Координаты"
  , deliverTo_map     :: F MapField    "deliverTo_map" ""
  }
  deriving Typeable


instance Model Continue where
  type TableName Continue = "continuetbl"
  type Parent Continue = Service
  modelInfo = mkModelInfo Continue ident
  modelView v = case parentView v :: Maybe (ModelView Continue) of
    Nothing -> Nothing
    Just mv -> Just
      $ modifyView (mv {mv_title = "Продолжение путешествия"})
      $ mapWidget deliverFrom_address deliverFrom_coords deliverFrom_map
      ++ mapWidget deliverTo_address deliverTo_coords deliverTo_map
