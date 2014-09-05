module Carma.Model.Service.Rent where

import Data.Aeson as Aeson
import Data.Text
import Data.Typeable

import Data.Model as Model
import Data.Model.View
import Carma.Model.LegacyTypes
import Carma.Model.CarClass (CarClass)
import Carma.Model.CarMake  (CarMake)
import Carma.Model.CarModel (CarModel)
import Carma.Model.Partner (Partner)
import Carma.Model.Service (Service)


data Rent = Rent
  { ident :: PK Int Rent ""
  , towDealer_partner   :: F (Maybe Text) "towDealer_partner" "Дилер"
  , towDealer_partnerId :: F (Maybe (IdentI Partner)) "towDealer_partnerId" ""
  , towDealer_address   :: F (Maybe Text) "towDealer_address" "Адрес"
  , towDealer_coords    :: F (Maybe Text) "towDealer_coords" "Координаты"
  , rentAddress_address :: F PickerField "rentAddress_address" "Адрес доставки"
  , rentAddress_comment :: F (Maybe Text) "rentAddress_comment" "Примечания"
  , rentAddress_coords  :: F PickerField "rentAddress_coords" "Координаты"
  , rentAddress_map     :: F MapField "rentAddress_map" ""
  , vinRent             :: F (Maybe Text) "vinRent" "VIN подменного автомобиля"
  , carClass            :: F (Maybe (IdentI CarClass)) "carClass" "Класс автомобиля"
  , providedFor         :: F (Maybe Text) "providedFor"
                           "Срок, на который предоставлен автомобиль (дней)"
  , rentedMake          :: F (Maybe (IdentI CarMake)) "rentedMake"
                           "Марка предоставленного автомобиля"
  , rentedModel         :: F (Maybe (IdentI CarModel)) "rentedModel"
                           "Модель предоставленного автомобиля"
  , orderNumber         :: F (Maybe Text) "orderNumber" "Номер заказ-наряда"
  }
  deriving Typeable


instance Model Rent where
  type TableName Rent = "renttbl"
  type Parent Rent = Service
  parentInfo = ExParent modelInfo

  modelInfo = mkModelInfo Rent ident
  modelView v = case parentView v :: Maybe (ModelView Rent) of
    Nothing -> Nothing
    Just mv -> Just
      $ modifyView (mv {mv_title = "Подменный автомобиль"})
      $ setType "dictionary" towDealer_partnerId
      : setMeta "dictionaryParent"
        (Aeson.String $ fieldName rentedMake) rentedModel
      : setMeta "widget" "partner" towDealer_partner
      : invisible towDealer_partnerId
      : invisible towDealer_coords
      : mapWidget rentAddress_address rentAddress_coords rentAddress_map
