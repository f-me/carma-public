
module Carma.Model.Service.Hotel where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.LegacyTypes
import Carma.Model.Service (Service)


data Hotel = Hotel
  { ident :: PK Int Hotel ""
  , caseAddress_address :: F PickerField "caseAddress_address" "Адрес кейса"
  , caseAddress_comment :: F Text        "caseAddress_comment" "Примечания"
  , caseAddress_coords  :: F PickerField "caseAddress_coords"  "Координаты"
  , caseAddress_map     :: F MapField    "caseAddress_map"     ""
  , providedFor         :: F Text        "providedFor"
                           "Срок, на который предоставлена гостиница (дней)"
  }
  deriving Typeable

instance Model Hotel where
  type TableName Hotel = "hoteltbl"
  type Parent Hotel = Service
  modelInfo = mkModelInfo Hotel ident
  modelView _ = (defaultView :: ModelView Hotel)
    {mv_title = "Гостиница"}
