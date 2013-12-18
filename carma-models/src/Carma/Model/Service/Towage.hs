
module Carma.Model.Service.Towage where

import Data.Text
import Data.Typeable
import Data.Time.Clock    (UTCTime)

import Data.Model
import Data.Model.View
import Carma.Model.Types()
import Carma.Model.LegacyTypes (PickerField, MapField)
import Carma.Model.Service (Service)
import Carma.Model.Search as S

data Towage = Towage
  { ident                    :: PK Int Towage ""
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
                             "Дилер (куда эвакуируют автомобиль)"
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


instance Model Towage where
  type TableName Towage = "towagetbl"
  type Parent Towage = Service
  modelInfo = mkModelInfo Towage ident
  modelView "search" = modifyView (searchView towageSearchParams)
    $ (setType "dictionary-set" towDealer_partnerId) : viewModifier
  modelView _ =
    modifyView (defaultView :: ModelView Towage) {mv_title = "Эвакуация"}
      $(setType "dictionary" towDealer_partnerId) : viewModifier

viewModifier =
  [dict towDealer_partnerId $ (dictOpt "allPartners")
          { dictType    = Just "ComputedDict"
          , dictBounded = True
          }
  ]


towageSearchParams :: [(Text, [Predicate Towage])]
towageSearchParams = [("towDealer_partnerId", listOf towDealer_partnerId)]
