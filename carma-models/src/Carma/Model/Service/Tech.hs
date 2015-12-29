module Carma.Model.Service.Tech where

import Data.Text
import Data.Typeable

import Data.Aeson((.=), object)
import Data.Model
import Data.Model.Types
import Data.Model.View
import Carma.Model.LegacyTypes (Checkbox)
import Carma.Model.Service (Service)
import Carma.Model.TechType (TechType)
import qualified Carma.Model.TechType as TechType


data Tech = Tech
  { ident          :: PK Int Tech ""
  , techType       :: F (Maybe (IdentI TechType)) "techType" "Услуга"
  , suburbanMilage :: F (Maybe Text) "suburbanMilage"
                      "Пробег техпомощи за городом"
  , orderNumber    :: F (Maybe Text) "orderNumber" "Номер заказ-наряда"
  , check1         :: F (Maybe Checkbox) "check1"
                      "Капот открывается"
  , check2         :: F (Maybe Checkbox) "check2"
                      "Наличие запасного колеса"
  , check3         :: F (Maybe Checkbox) "check3"
                      "Наличие секреток"
  , check4         :: F (Maybe Checkbox) "check4"
                      "Запасной ключ имеется"
  , check5         :: F (Maybe Checkbox) "check5"
                      "Документы на автомобиль на руках"
  , check6         :: F (Maybe Checkbox) "check6"
                      "Не открывается лючок бензобака"
  }
  deriving Typeable

instance Model Tech where
  type TableName Tech = "techtbl"
  type Parent Tech = Service
  parentInfo = ExParent modelInfo
  modelInfo = mkModelInfo Tech ident
  modelView v = case parentView v :: Maybe (ModelView Tech) of
    Nothing -> Nothing
    Just mv ->
      Just $ modifyView (mv {mv_title = "Техпомощь"})
        [setMeta "visibleIf" (object
          ["techType" .= [TechType.charge, TechType.chargeRuamc]
          ]) check1
        ,setMeta "visibleIf" (object
          ["techType" .= [TechType.wheel, TechType.wheelTower,  TechType.wheelRuamc]
          ]) check2
        ,setMeta "visibleIf" (object
          ["techType" .= [TechType.wheel, TechType.wheelTower,  TechType.wheelRuamc]
          ]) check3
        ,setMeta "visibleIf" (object
          ["techType" .= [TechType.hack, TechType.hackByRuamc]
          ]) check4
        ,setMeta "visibleIf" (object
          ["techType" .= [TechType.hack, TechType.hackByRuamc]
          ]) check5
        ,setMeta "visibleIf" (object
          ["techType" .= [TechType.fuel, TechType.fuelTower, TechType.fuelRuamc]
          ]) check6
        ]
