
module Carma.Model.Service.Taxi where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.LegacyTypes
import Carma.Model.Service (Service)

data Taxi = Taxi
  { ident :: PK Int Taxi ""
  , taxiFrom_address :: F PickerField "taxiFrom_address" "Откуда везти"
  , taxiFrom_comment :: F Text        "taxiFrom_comment" "Примечания"
  , taxiFrom_coords  :: F PickerField "taxiFrom_coords" "Координаты"
  , taxiFrom_map     :: F MapField    "taxiFrom_map" ""

  , taxiTo_address :: F PickerField "taxiTo_address" "Куда везти"
  , taxiTo_comment :: F Text        "taxiTo_comment" "Примечания"
  , taxiTo_coords  :: F PickerField "taxiTo_coords" "Координаты"
  , taxiTo_map     :: F MapField    "taxiTo_map" ""
  }
  deriving Typeable


instance Model Taxi where
  type TableName Taxi = "taxitbl"
  type Parent Taxi = Service
  modelInfo = mkModelInfo Taxi ident
  modelView _ = modifyView
    (defaultView :: ModelView Taxi) {mv_title = "Такси"}
    $ mapWidget taxiFrom_address taxiFrom_coords taxiFrom_map
    ++ mapWidget taxiTo_address taxiTo_coords taxiTo_map
