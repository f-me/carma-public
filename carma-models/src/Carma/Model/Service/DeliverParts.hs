module Carma.Model.Service.DeliverParts where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.LegacyTypes
import Carma.Model.Service (Service)


data DeliverParts = DeliverParts
  { ident :: PK Int DeliverParts ""
  , parts             :: F (Maybe Text)"parts"             "Запчасти"
  , toAddress_address :: F PickerField "toAddress_address" "Куда везти"
  , toAddress_comment :: F (Maybe Text)"toAddress_comment" "Примечания"
  , toAddress_coords  :: F PickerField "toAddress_coords"  "Координаты"
  , toAddress_map     :: F MapField    "toAddress_map"     ""
  }
  deriving Typeable

instance Model DeliverParts where
  type TableName DeliverParts = "deliverpartstbl"
  type Parent DeliverParts = Service
  parentInfo = ExParent modelInfo
  modelInfo = mkModelInfo DeliverParts ident
  modelView v = case parentView v :: Maybe (ModelView DeliverParts) of
    Nothing -> Nothing
    Just mv -> Just
      $ modifyView (mv {mv_title = "Доставка запчастей"})
      $ mapWidget toAddress_address toAddress_coords toAddress_map
