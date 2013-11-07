
module Carma.Model.Service.Towage where

import Control.Applicative
import Data.Text
import Data.Typeable
import Data.Time.Clock    (UTCTime)
import Data.Aeson
import Database.PostgreSQL.Simple.ToField   (ToField(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))

import Data.Model
import Data.Model.View
import Data.Model.Types
import Data.Model.CoffeeType
import Carma.Model.Service (Service)


data Towage = Towage
  { ident                    :: PK Int Towage
  , towerType                :: F Text {-FIXME-} "towerType"
                             "Тип эвакуатора"
  , towType                  :: F Text {-FIXME-} "towType"
                             "Вид эвакуации"
  , vandalism                :: F Bool "vandalism"
                             "Случай вандализма"
  , accident                 :: F Bool "accident"
                             "ДТП"
  , towDealer_partner        :: F Text "towDealer_partner"
                             "Дилер (куда эвакуируют автомобиль)"
  , towDealer_partnerId      :: F Text "towDealer_partnerId"
                             ""
  , towDealer_address        :: F Text "towDealer_address"
                             "Адрес"
  , towDealer_coords         :: F Text "towDealer_coords"
                             "Координаты"
  , dealerDistance           :: F Text "dealerDistance"
                             "Расстояние до дилера"
  , towAddress_address       :: F PickerField
                             "towAddress_address" "Адрес доставки"
  , towAddress_comment       :: F Text "towAddress_comment"
                             "Примечания"
  , towAddress_coords        :: F PickerField "towAddress_coords"
                             "Координаты"
  , towAddress_map           :: F MapField "towAddress_map"
                             ""

  -- insert [contractor_*, marginalCost] here

  , towerAddress_address     :: F PickerField
                             "towerAddress_address" "Адрес выезда эвакуатора"
  , towerAddress_comment     :: F Text "towerAddress_comment"
                             "Примечания"
  , towerAddress_coords      :: F PickerField
                             "towerAddress_coords" "Координаты"
  , towerAddress_map         :: F MapField "towerAddress_map"
                             ""
  , wheelsUnblocked          :: F Text {-FIXME-} "wheelsUnblocked"
                             "Количество заблокированных колёс"
  , canNeutral               :: F Bool "canNeutral"
                             "Переключается на нейтральную передачу"
  , towingPointPresent       :: F Bool "towingPointPresent"
                             "Есть буксировочный крюк"
  , manipulatorPossible      :: F Bool "manipulatorPossible"
                             "Есть место для манипулятора"
  , companion                :: F Bool "companion"
                             "Клиент/Доверенное лицо будет сопровождать автомобиль"
  , suburbanMilage           :: F Text "suburbanMilage"
                             "Пробег эвакуатора за городом"
  , orderNumber              :: F Text "orderNumber"
                             "Номер заказ-наряда"
  , repairEndDate            :: F UTCTime {-FIXME: day-} "repairEndDate"
                             "Дата окончания ремонта"

  -- insert {paid,scan,..} here
  }
  deriving Typeable



data PickerField = PickerField Text deriving Typeable
instance CoffeeType PickerField where
  coffeeType = Wrap "picker"
instance FromJSON PickerField where
  parseJSON fld = PickerField <$> parseJSON fld
instance ToJSON PickerField where
  toJSON (PickerField txt) = toJSON txt
instance ToField PickerField where
  toField (PickerField txt) = toField txt
instance FromField PickerField where
  fromField fld m = PickerField <$> fromField fld m

data MapField = MapField Text deriving Typeable
instance CoffeeType MapField where
  coffeeType = Wrap "map"
instance FromJSON MapField where
  parseJSON fld = MapField <$> parseJSON fld
instance ToJSON MapField where
  toJSON (MapField txt) = toJSON txt
instance ToField MapField where
  toField (MapField txt) = toField txt
instance FromField MapField where
  fromField fld m = MapField <$> fromField fld m


instance Model Towage where
  type TableName Towage = "towagetbl"
  type Parent Towage = Service
  modelInfo = mkModelInfo Towage ident
  modelView _ = defaultView

