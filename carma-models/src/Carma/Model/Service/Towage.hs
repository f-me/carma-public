module Carma.Model.Service.Towage where

import Data.Text
import Data.Time
import Data.Typeable
import Data.Aeson((.=), object)
import Data.Scientific

import Data.Model
import Data.Model.Types
import Data.Model.View
import Carma.Model.Types()
import Carma.Model.LegacyTypes
import Carma.Model.Partner (Partner)
import Carma.Model.Service (Service)
import Carma.Model.Search as S
import Carma.Model.TowType (TowType)
import Carma.Model.TowerType (TowerType)

data Towage = Towage
  { ident                    :: PK Int Towage ""
  , towerType                :: F (Maybe (IdentI TowerType)) "towerType"
                             "Тип эвакуатора"
  , towType                  :: F (Maybe (IdentI TowType)) "towType"
                             "Вид эвакуации"
  , vandalism                :: F (Maybe Checkbox) "vandalism"
                             "Случай вандализма"
  , accident                 :: F (Maybe Checkbox) "accident"
                             "ДТП"
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
  , wheelsBlocked
    :: F (Maybe Int)
       "wheelsBlocked" "Количество заблокированных колёс"
  , canNeutral               :: F (Maybe Checkbox) "canNeutral"
                             "Переключается на нейтральную передачу"
  , towingPointPresent       :: F (Maybe Checkbox) "towingPointPresent"
                             "Есть буксировочный крюк"
  , manipulatorPossible      :: F (Maybe Checkbox) "manipulatorPossible"
                             "Есть место для манипулятора"
  , companion                :: F (Maybe Checkbox) "companion"
                             "Клиент/Доверенное лицо будет сопровождать автомобиль"
  , check1                   :: F (Maybe Checkbox) "check1"
                             "Заблокирован электронный ручной тормоз"
  , check2                   :: F (Maybe Checkbox) "check2"
                             "Руль заблокирован"
  , orderNumber              :: F (Maybe Text) "orderNumber"
                             "Номер заказ-наряда"
  , repairEndDate            :: F (Maybe Day) "repairEndDate"
                             "Дата окончания ремонта"
  , isCountryRide            :: F Bool "isCountryRide" "За городом"
  , suburbanMilage           :: F (Maybe Scientific) "suburbanMilage" "Пробег за городом"
  , totalMilage              :: F (Maybe Scientific) "totalMilage" "Километраж по тахометру"
  , partnerWarnedInTime      :: F (Maybe Bool) "partnerWarnedInTime" "Партнёр предупредил вовремя"
  }
  deriving Typeable


instance Model Towage where
  type TableName Towage = "towagetbl"
  type Parent Towage = Service
  parentInfo = ExParent modelInfo
  modelInfo = mkModelInfo Towage ident
  modelView = \case
    "search" -> Just
      $ modifyView (searchView towageSearchParams)
      $ (setType "dictionary-set-int" towDealer_partnerId)
      : invisible partnerWarnedInTime
      : viewModifier
    ""
      -> case parentView "" :: Maybe (ModelView Towage) of
        Nothing -> Nothing
        Just mv -> Just $ modifyView (mv {mv_title = "Эвакуация"}) viewModifier'
    _ -> Nothing


viewModifier' :: [(Text, FieldView -> FieldView) :@ Towage]
viewModifier'
  = setType "dictionary" towDealer_partnerId
  : setMeta "group-widget" "partner" towDealer_partner
  : invisible towDealer_partnerId
  : invisible towDealer_coords
  : setMeta "visibleIf" (object ["isCountryRide" .= [True]]) suburbanMilage
  : setMeta "visibleIf" (object ["isCountryRide" .= [True]]) totalMilage
  : setMeta "visibleIf" (object ["isCountryRide" .= [True]]) partnerWarnedInTime
  : widget "partnerWarnedInTime-btn" partnerWarnedInTime
  : viewModifier


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
