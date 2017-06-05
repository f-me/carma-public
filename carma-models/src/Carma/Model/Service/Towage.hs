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
import Carma.Model.TowSort (TowSort)
import Carma.Model.TowType (TowType)
import Carma.Model.TowerType (TowerType)

data Towage = Towage
  { ident                    :: PK Int Towage ""
  , towType                  :: F (Maybe (IdentI TowType)) "towType"
                             "Вид эвакуации"
  , towSort                  :: F (Maybe (IdentI TowSort)) "towSort"
                             "Тип эвакуации"
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

-- I wish we could just use classy lenses here
class TowageService a where
  g_towDealer_partnerId  :: a -> F (Maybe (IdentI Partner))
                            "towDealer_partnerId"
                            "Дилер (куда эвакуируют автомобиль)"
  g_towDealer_partner    :: a -> F (Maybe Text) "towDealer_partner"
                            "Дилер (куда эвакуируют автомобиль)"
  g_towDealer_coords     :: a -> F (Maybe Text) "towDealer_coords"
                            "Координаты"

  g_towAddress_address   :: a -> F PickerField
                            "towAddress_address" "Адрес доставки"
  g_towAddress_coords    :: a ->  F PickerField "towAddress_coords"
                            "Координаты"

  g_towAddress_map       :: a -> F (Maybe MapField) "towAddress_map"
                           ""

  g_towerAddress_address :: a -> F (Maybe PickerField)
                            "towerAddress_address" "Адрес выезда эвакуатора"
  g_towerAddress_coords  :: a -> F (Maybe PickerField)
                            "towerAddress_coords" "Координаты"
  g_towerAddress_map     :: a -> F (Maybe MapField) "towerAddress_map"
                            ""

  g_suburbanMilage       :: a -> F (Maybe Scientific) "suburbanMilage"
                            "Пробег за городом"
  g_totalMilage          :: a -> F (Maybe Scientific) "totalMilage"
                            "Километраж по тахометру"
  g_partnerWarnedInTime  :: a -> F (Maybe Bool) "partnerWarnedInTime"
                            "Партнёр предупредил вовремя"


instance TowageService Towage where
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


viewModifier' :: TowageService a => [(Text, FieldView -> FieldView) :@ a]
viewModifier'
  = setType "dictionary" g_towDealer_partnerId
  : setMeta "group-widget" "partner" g_towDealer_partner
  : invisible g_towDealer_partnerId
  : invisible g_towDealer_coords
  : setMeta "visibleIf" (object ["isCountryRide" .= [True]]) g_suburbanMilage
  : setMeta "visibleIf" (object ["isCountryRide" .= [True]]) g_totalMilage
  : setMeta "visibleIf" (object ["isCountryRide" .= [True]]) g_partnerWarnedInTime
  : widget "partnerWarnedInTime-btn" g_partnerWarnedInTime
  : viewModifier


viewModifier :: TowageService a => [(Text, FieldView -> FieldView) :@ a]
viewModifier =
  [dict g_towDealer_partnerId $ (dictOpt "allPartners")
          { dictType    = Just "ComputedDict"
          , dictBounded = True
          }
  ]
  ++ mapWidget g_towAddress_address g_towAddress_coords g_towAddress_map
  ++ mapWidget g_towerAddress_address g_towerAddress_coords g_towerAddress_map

towageSearchParams :: [(Text, [Predicate Towage])]
towageSearchParams = [("towDealer_partnerId", listOf towDealer_partnerId)]
