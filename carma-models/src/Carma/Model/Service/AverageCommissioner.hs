module Carma.Model.Service.AverageCommissioner where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.Types()
import Carma.Model.LegacyTypes

import Carma.Model.Activity    (Activity)
import Carma.Model.RequestType (RequestType)
import Carma.Model.Service     (Service)


data AverageCommissioner = AverageCommissioner
  { ident :: PK Int AverageCommissioner ""
  , requestType :: F (Maybe (IdentI RequestType))
                   "requestType" "Тип запроса"
  , whatToSay1  :: F (Maybe Text)
                   "whatToSay1" "Описание проблемы"
  , activity    :: F (Maybe (IdentI Activity))
                   "activity" "Тип действия"
  , commAddress_address
                :: F PickerField
                   "commAddress_address" "Адрес выезда аваркома"
  , commAddress_comment
                :: F (Maybe Text)
                   "commAddress_comment" "Примечания"
  , commAddress_coords
                :: F PickerField
                   "commAddress_coords" "Координаты"
  , commAddress_map
                :: F MapField "commAddress_map" ""
  , commMilage  :: F (Maybe Text) {- FIXME: why not int? -}
                   "commMilage" "Пробег аваркома за городом"
  }
  deriving Typeable


instance Model AverageCommissioner where
  type TableName AverageCommissioner = "averagecommissionertbl"
  type Parent AverageCommissioner = Service
  parentInfo = ExParent modelInfo
  modelInfo = mkModelInfo AverageCommissioner ident
  modelView v = case parentView v :: Maybe (ModelView AverageCommissioner) of
    Nothing -> Nothing
    Just mv -> Just
      $ modifyView (mv {mv_title = "Аварийный комиссар"})
      $ mapWidget commAddress_address commAddress_coords commAddress_map
