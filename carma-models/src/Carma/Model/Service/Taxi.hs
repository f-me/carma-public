module Carma.Model.Service.Taxi where

import Data.Text
import Data.Typeable
import Data.Aeson((.=), object)
import Data.Scientific

import Data.Model
import Data.Model.View
import Carma.Model.LegacyTypes
import Carma.Model.Service (Service)

data Taxi = Taxi
  { ident :: PK Int Taxi ""
  , taxiFrom_address :: F PickerField "taxiFrom_address" "Откуда везти"
  , taxiFrom_comment :: F (Maybe Text)"taxiFrom_comment" "Примечания"
  , taxiFrom_coords  :: F PickerField "taxiFrom_coords" "Координаты"
  , taxiFrom_map     :: F MapField    "taxiFrom_map" ""

  , taxiTo_address :: F PickerField "taxiTo_address" "Куда везти"
  , taxiTo_comment :: F (Maybe Text)"taxiTo_comment" "Примечания"
  , taxiTo_coords  :: F PickerField "taxiTo_coords" "Координаты"
  , taxiTo_map     :: F MapField    "taxiTo_map" ""
  , isCountryRide       :: F Bool "isCountryRide" "За городом"
  , suburbanMilage      :: F (Maybe Scientific) "suburbanMilage" "Пробег за городом"
  , totalMilage         :: F (Maybe Scientific) "totalMilage" "Километраж по тахометру"
  , partnerWarnedInTime :: F (Maybe Bool) "partnerWarnedInTime" "Партнёр предупредил вовремя"
  }
  deriving Typeable


instance Model Taxi where
  type TableName Taxi = "taxitbl"
  type Parent Taxi = Service
  parentInfo = ExParent modelInfo
  modelInfo = mkModelInfo Taxi ident
  modelView v = case parentView v :: Maybe (ModelView Taxi) of
    Nothing -> Nothing
    Just mv -> Just
      $ modifyView (mv {mv_title = "Такси"})
      $ setMeta "visibleIf" (object ["isCountryRide" .= [True]]) suburbanMilage
      : setMeta "visibleIf" (object ["isCountryRide" .= [True]]) totalMilage
      : setMeta "visibleIf" (object ["isCountryRide" .= [True]]) partnerWarnedInTime
      : widget "partnerWarnedInTime-btn" partnerWarnedInTime
      : mapWidget taxiFrom_address taxiFrom_coords taxiFrom_map
      ++ mapWidget taxiTo_address taxiTo_coords taxiTo_map
