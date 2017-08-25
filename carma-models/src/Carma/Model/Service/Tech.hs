module Carma.Model.Service.Tech where

import Data.Text
import Data.Typeable
import Data.Scientific

import Data.Aeson((.=), object)
import Data.Model
import Data.Model.View
import Carma.Model.LegacyTypes (Checkbox)
import Carma.Model.Service (Service)
import Carma.Model.TechType (TechType)
import qualified Carma.Model.TechType as TechType


data Tech = Tech
  { ident          :: PK Int Tech ""
  , techType       :: F (Maybe (IdentI TechType)) "techType" "Услуга"
  , orderNumber    :: F (Maybe Text) "orderNumber" "Номер заказ-наряда"
  , isCountryRide  :: F Bool "isCountryRide" "За городом"

  -- Naming scheme convention: complNpM, where N is the id of a
  -- TechType dictionary entry. Actual visibility rules are set via
  -- metas on these field using TechType idents (see below).
  , compl27p1 :: F (Maybe Checkbox) "compl27p1" "compl27p1"
  , compl27p2 :: F (Maybe Checkbox) "compl27p2" "compl27p2"
  , compl27p3 :: F (Maybe Checkbox) "compl27p3" "compl27p3"
  , compl27p4 :: F (Maybe Checkbox) "compl27p4" "compl27p4"
  , compl27p5 :: F (Maybe Checkbox) "compl27p5" "compl27p5"
  , compl29p1 :: F (Maybe Checkbox) "compl29p1" "compl29p1"
  , compl29p2 :: F (Maybe Checkbox) "compl29p2" "compl29p2"
  , compl29p3 :: F (Maybe Checkbox) "compl29p3" "compl29p3"
  , compl29p4 :: F (Maybe Checkbox) "compl29p4" "compl29p4"
  , compl29p5 :: F (Maybe Checkbox) "compl29p5" "compl29p5"
  , compl28p1 :: F (Maybe Checkbox) "compl28p1" "compl28p1"
  , compl28p2 :: F (Maybe Checkbox) "compl28p2" "compl28p2"
  , compl28p3 :: F (Maybe Checkbox) "compl28p3" "compl28p3"
  , compl28p4 :: F (Maybe Checkbox) "compl28p4" "compl28p4"
  , compl28p5 :: F (Maybe Checkbox) "compl28p5" "compl28p5"
  , compl32p1 :: F (Maybe Checkbox) "compl32p1" "compl32p1"
  , compl32p2 :: F (Maybe Checkbox) "compl32p2" "compl32p2"
  , compl32p3 :: F (Maybe Checkbox) "compl32p3" "compl32p3"
  , compl32p4 :: F (Maybe Checkbox) "compl32p4" "compl32p4"
  , compl32p5 :: F (Maybe Checkbox) "compl32p5" "compl32p5"
  , compl33p1 :: F (Maybe Checkbox) "compl33p1" "compl33p1"
  , compl33p2 :: F (Maybe Checkbox) "compl33p2" "compl33p2"
  , compl33p3 :: F (Maybe Checkbox) "compl33p3" "compl33p3"
  , compl33p4 :: F (Maybe Checkbox) "compl33p4" "compl33p4"
  , compl33p5 :: F (Maybe Checkbox) "compl33p5" "compl33p5"
  , compl31p1 :: F (Maybe Checkbox) "compl31p1" "compl31p1"
  , compl31p2 :: F (Maybe Checkbox) "compl31p2" "compl31p2"
  , compl31p3 :: F (Maybe Checkbox) "compl31p3" "compl31p3"
  , compl31p4 :: F (Maybe Checkbox) "compl31p4" "compl31p4"
  , compl31p5 :: F (Maybe Checkbox) "compl31p5" "compl31p5"
  , compl35p1 :: F (Maybe Checkbox) "compl35p1" "compl35p1"
  , compl35p2 :: F (Maybe Checkbox) "compl35p2" "compl35p2"
  , compl35p3 :: F (Maybe Checkbox) "compl35p3" "compl35p3"
  , compl35p4 :: F (Maybe Checkbox) "compl35p4" "compl35p4"
  , compl35p5 :: F (Maybe Checkbox) "compl35p5" "compl35p5"
  , compl34p1 :: F (Maybe Checkbox) "compl34p1" "compl34p1"
  , compl34p2 :: F (Maybe Checkbox) "compl34p2" "compl34p2"
  , compl34p3 :: F (Maybe Checkbox) "compl34p3" "compl34p3"
  , compl34p4 :: F (Maybe Checkbox) "compl34p4" "compl34p4"
  , compl34p5 :: F (Maybe Checkbox) "compl34p5" "compl34p5"
  , compl37p1 :: F (Maybe Checkbox) "compl37p1" "compl37p1"
  , compl37p2 :: F (Maybe Checkbox) "compl37p2" "compl37p2"
  , compl37p3 :: F (Maybe Checkbox) "compl37p3" "compl37p3"
  , compl37p4 :: F (Maybe Checkbox) "compl37p4" "compl37p4"
  , compl37p5 :: F (Maybe Checkbox) "compl37p5" "compl37p5"
  , compl36p1 :: F (Maybe Checkbox) "compl36p1" "compl36p1"
  , compl36p2 :: F (Maybe Checkbox) "compl36p2" "compl36p2"
  , compl36p3 :: F (Maybe Checkbox) "compl36p3" "compl36p3"
  , compl36p4 :: F (Maybe Checkbox) "compl36p4" "compl36p4"
  , compl36p5 :: F (Maybe Checkbox) "compl36p5" "compl36p5"
  , compl41p1 :: F (Maybe Checkbox) "compl41p1" "compl41p1"
  , compl41p2 :: F (Maybe Checkbox) "compl41p2" "compl41p2"
  , compl41p3 :: F (Maybe Checkbox) "compl41p3" "compl41p3"
  , compl41p4 :: F (Maybe Checkbox) "compl41p4" "compl41p4"
  , compl41p5 :: F (Maybe Checkbox) "compl41p5" "compl41p5"

  , suburbanMilage      :: F (Maybe Scientific) "suburbanMilage" "Пробег за городом"
  , totalMilage         :: F (Maybe Scientific) "totalMilage" "Километраж по тахометру"
  , partnerWarnedInTime :: F (Maybe Bool) "partnerWarnedInTime" "Партнёр предупредил вовремя"
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
        [ setMeta "filterBy" "isActive" techType

        , setMeta "visibleIf" (object ["isCountryRide" .= [True]]) suburbanMilage
        , setMeta "visibleIf" (object ["isCountryRide" .= [True]]) totalMilage
        , setMeta "visibleIf" (object ["isCountryRide" .= [True]]) partnerWarnedInTime

        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_27]]) compl27p1
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_27]]) compl27p2
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_27]]) compl27p3
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_27]]) compl27p4
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_27]]) compl27p5

        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_29]]) compl29p1
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_29]]) compl29p2
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_29]]) compl29p3
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_29]]) compl29p4
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_29]]) compl29p5

        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_28]]) compl28p1
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_28]]) compl28p2
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_28]]) compl28p3
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_28]]) compl28p4
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_28]]) compl28p5

        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_32]]) compl32p1
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_32]]) compl32p2
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_32]]) compl32p3
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_32]]) compl32p4
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_32]]) compl32p5

        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_33]]) compl33p1
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_33]]) compl33p2
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_33]]) compl33p3
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_33]]) compl33p4
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_33]]) compl33p5

        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_31]]) compl31p1
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_31]]) compl31p2
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_31]]) compl31p3
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_31]]) compl31p4
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_31]]) compl31p5

        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_35]]) compl35p1
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_35]]) compl35p2
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_35]]) compl35p3
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_35]]) compl35p4
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_35]]) compl35p5

        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_34]]) compl34p1
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_34]]) compl34p2
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_34]]) compl34p3
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_34]]) compl34p4
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_34]]) compl34p5

        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_37]]) compl37p1
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_37]]) compl37p2
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_37]]) compl37p3
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_37]]) compl37p4
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_37]]) compl37p5

        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_36]]) compl36p1
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_36]]) compl36p2
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_36]]) compl36p3
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_36]]) compl36p4
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_36]]) compl36p5

        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_41]]) compl41p1
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_41]]) compl41p2
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_41]]) compl41p3
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_41]]) compl41p4
        , setMeta "visibleIf" (object ["techType" .= [TechType.customTech_41]]) compl41p5

        , widget "partnerWarnedInTime-btn" partnerWarnedInTime
        ]
