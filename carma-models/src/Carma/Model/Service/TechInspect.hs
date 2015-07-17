module Carma.Model.Service.TechInspect where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Activity    (Activity)
import Carma.Model.RequestType (RequestType)
import Carma.Model.Service     (Service)


data TechInspect = TechInspect
  { ident       :: PK Int TechInspect ""
  , requestType :: F (Maybe (IdentI RequestType)) "requestType" "Тип запроса"
  , whatToSay1  :: F (Maybe Text) "whatToSay1" "Описание проблемы"
  , activity    :: F (Maybe (IdentI Activity)) "activity" "Тип действия"
  }
  deriving Typeable

instance Model TechInspect where
  type TableName TechInspect = "tech1tbl"
  type Parent TechInspect = Service
  parentInfo = ExParent modelInfo
  modelInfo = mkModelInfo TechInspect ident
  modelView v = case parentView v :: Maybe (ModelView TechInspect) of
    Nothing -> Nothing
    Just mv -> Just $ modifyView (mv {mv_title = "TO"}) [textarea whatToSay1]
