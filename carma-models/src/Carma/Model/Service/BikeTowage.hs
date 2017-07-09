module Carma.Model.Service.BikeTowage where

import Data.Text
import Data.Time
import Data.Typeable
import Data.Scientific

import Data.Model
import Data.Model.View
import Carma.Model.Types()
import Carma.Model.BikeTowType (BikeTowType)
import Carma.Model.LegacyTypes
import Carma.Model.Partner (Partner)
import Carma.Model.Service (Service)
import qualified Carma.Model.Service.Towage as T

data BikeTowage = BikeTowage
  { ident                    :: PK Int BikeTowage ""
  , towDealer_partner        :: F (Maybe Text) "towDealer_partner"
                             "Дилер (куда эвакуируют автомобиль)"
  , towDealer_partnerId      :: F (Maybe (IdentI Partner)) "towDealer_partnerId"
                             "Дилер (куда эвакуируют автомобиль)"
  , towDealer_address        :: F (Maybe Text) "towDealer_address"
                             "Адрес"
  , towDealer_coords         :: F (Maybe Text) "towDealer_coords"
                             "Координаты"
  , dealerDistance           :: F (Maybe Text) "dealerDistance"
                             "Расстояние до дилера"
  , towAddress_address       :: F PickerField
                             "towAddress_address" "Адрес доставки"
  , towAddress_comment       :: F (Maybe Text) "towAddress_comment"
                             "Примечания"
  , towAddress_coords        :: F PickerField "towAddress_coords"
                             "Координаты"
  , towAddress_map           :: F (Maybe MapField) "towAddress_map"
                             ""
  , towerAddress_address     :: F (Maybe PickerField)
                             "towerAddress_address" "Адрес выезда эвакуатора"
  , towerAddress_comment     :: F (Maybe Text) "towerAddress_comment"
                             "Примечания"
  , towerAddress_coords      :: F (Maybe PickerField)
                             "towerAddress_coords" "Координаты"
  , towerAddress_map         :: F (Maybe MapField) "towerAddress_map"
                             ""
  , orderNumber              :: F (Maybe Text) "orderNumber"
                             "Номер заказ-наряда"
  , repairEndDate            :: F (Maybe Day) "repairEndDate"
                             "Дата окончания ремонта"
  , isCountryRide            :: F Bool "isCountryRide" "За городом"
  , canPushBike              :: F Bool "canPushBike" "Возможно ли катить мотоцикл"
  , canUnblock               :: F Bool "canUnblock" "Возможно ли разблокировать руль"
  , canHelp                  :: F Bool "canHelp" "Сможете ли помочь при погрузке и креплении"
  , compl4                   :: F Bool "compl4" "Сложность №4"
  , compl5                   :: F Bool "compl5" "Сложность №5"
  , bikeTowType              :: F (Maybe (IdentI BikeTowType)) "bikeTowType"
                             "Тип мотоэвакуации"
  , suburbanMilage           :: F (Maybe Scientific) "suburbanMilage" "Пробег за городом"
  , totalMilage              :: F (Maybe Scientific) "totalMilage" "Километраж по тахометру"
  , partnerWarnedInTime      :: F (Maybe Bool) "partnerWarnedInTime" "Партнёр предупредил вовремя"
  }
  deriving Typeable


instance T.TowageService BikeTowage where
  g_towDealer_partnerId = towDealer_partnerId
  g_towDealer_partner = towDealer_partner
  g_towDealer_coords = towDealer_coords

  g_towAddress_address = towAddress_address
  g_towAddress_coords = towAddress_coords
  g_towAddress_map = towAddress_map

  g_towerAddress_address = towerAddress_address
  g_towerAddress_coords = towerAddress_coords
  g_towerAddress_map = towerAddress_map

  g_suburbanMilage = suburbanMilage
  g_totalMilage = totalMilage
  g_partnerWarnedInTime = partnerWarnedInTime


instance Model BikeTowage where
  type TableName BikeTowage = "BikeTowage"
  type Parent BikeTowage = Service
  parentInfo = ExParent modelInfo
  modelInfo = mkModelInfo BikeTowage ident
  modelView v = case parentView v :: Maybe (ModelView BikeTowage) of
    Nothing -> Nothing
    Just mv -> Just $ modifyView (mv {mv_title = "Мотоэвакуация"}) T.viewModifier'
