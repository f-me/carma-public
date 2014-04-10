
module Carma.Model.Service.Towage where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.Types
import Data.Model.View
import Carma.Model.Types()
import Carma.Model.LegacyTypes
import Carma.Model.Service (Service)
import Carma.Model.Search as S

data Towage = Towage
  { ident                    :: PK Int Towage ""
  , towerType                :: F (Maybe (IdentT TowerTypes)) "towerType"
                             "Тип эвакуатора"
  , towType                  :: F (Maybe (IdentT TowTypes)) "towType"
                             "Вид эвакуации"
  , vandalism                :: F (Maybe Checkbox) "vandalism"
                             "Случай вандализма"
  , accident                 :: F (Maybe Checkbox) "accident"
                             "ДТП"
  , towDealer_partner        :: F (Maybe Text) "towDealer_partner"
                             "Дилер (куда эвакуируют автомобиль)"
  , towDealer_partnerId      :: F (Maybe Text) "towDealer_partnerId"
                             "Дилер (куда эвакуируют автомобиль)"
  , towDealer_address        :: F (Maybe Text) "towDealer_address"
                             "Адрес"
  , towDealer_coords         :: F (Maybe Text) "towDealer_coords"
                             "Координаты"
  , dealerDistance           :: F (Maybe Text) "dealerDistance"
                             "Расстояние до дилера"
  , towAddress_address       :: F (Maybe PickerField)
                             "towAddress_address" "Адрес доставки"
  , towAddress_comment       :: F (Maybe Text) "towAddress_comment"
                             "Примечания"
  , towAddress_coords        :: F (Maybe PickerField) "towAddress_coords"
                             "Координаты"
  , towAddress_map           :: F (Maybe MapField) "towAddress_map"
                             ""

  -- insert [contractor_*, marginalCost] here

  , towerAddress_address     :: F (Maybe PickerField)
                             "towerAddress_address" "Адрес выезда эвакуатора"
  , towerAddress_comment     :: F (Maybe Text) "towerAddress_comment"
                             "Примечания"
  , towerAddress_coords      :: F (Maybe PickerField)
                             "towerAddress_coords" "Координаты"
  , towerAddress_map         :: F (Maybe MapField) "towerAddress_map"
                             ""
  , wheelsUnblocked
    :: F (Maybe (IdentT (WheelsBlockedCount)))
       "wheelsUnblocked" "Количество заблокированных колёс"
  , canNeutral               :: F (Maybe Checkbox) "canNeutral"
                             "Переключается на нейтральную передачу"
  , towingPointPresent       :: F (Maybe Checkbox) "towingPointPresent"
                             "Есть буксировочный крюк"
  , manipulatorPossible      :: F (Maybe Checkbox) "manipulatorPossible"
                             "Есть место для манипулятора"
  , companion                :: F (Maybe Checkbox) "companion"
                             "Клиент/Доверенное лицо будет сопровождать автомобиль"
  , suburbanMilage           :: F (Maybe Text) "suburbanMilage"
                             "Пробег эвакуатора за городом"
  , orderNumber              :: F (Maybe Text) "orderNumber"
                             "Номер заказ-наряда"
  , repairEndDate            :: F (Maybe LegacyDate) {-FIXME: day-} "repairEndDate"
                             "Дата окончания ремонта"

  -- insert {paid,scan,..} here
  }
  deriving Typeable


instance Model Towage where
  type TableName Towage = "towagetbl"
  type Parent Towage = Service
  modelInfo = mkModelInfo Towage ident
  modelView = \case
    "search" -> Just
      $ modifyView (searchView towageSearchParams)
      $ (setType "dictionary-set" towDealer_partnerId) : viewModifier
    "full"   -> modelView "new"
    "new"    -> Just
      $ modifyView (defaultView :: ModelView Towage) {mv_title = "Эвакуация"}
        $(setType "dictionary" towDealer_partnerId)
        : invisible towDealer_partnerId
        : viewModifier
    ""       -> modelView "full"
    _ -> Nothing

viewModifier :: [(Text, FieldView -> FieldView) :@ Towage]
viewModifier =
  [dict towDealer_partnerId $ (dictOpt "allPartners")
          { dictType    = Just "ComputedDict"
          , dictBounded = True
          }
  ]
  ++ mapWidget towAddress_address towAddress_coords towAddress_map
  ++ mapWidget towerAddress_address towerAddress_coords towerAddress_map


towageSearchParams :: [(Text, [Predicate Towage])]
towageSearchParams = [("towDealer_partnerId", listOf towDealer_partnerId)]
