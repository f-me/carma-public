
module Carma.Model.Service.TechInspect where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.Service (Service)
import Carma.Model.LegacyTypes


data TechInspect = TechInspect
  { ident       :: PK Int TechInspect ""
  , requestType :: F (Maybe (IdentT RequestType)) "requestType" "Тип запроса"
  , whatToSay1  :: F Text "whatToSay1" "Описание проблемы"
  , activity    :: F (Maybe (IdentT Activity)) "activity" "Тип действия"
  }
  deriving Typeable

instance Model TechInspect where
  type TableName TechInspect = "tech1"
  type Parent TechInspect = Service
  modelInfo = mkModelInfo TechInspect ident
  modelView _ = modifyView
    (defaultView :: ModelView TechInspect) {mv_title = "TO"}
    [textarea whatToSay1]

