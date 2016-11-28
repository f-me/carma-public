module Carma.Model.Service.LegalAssistance where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Activity    (Activity)
import Carma.Model.RequestType (RequestType)
import Carma.Model.Service     (Service)


data LegalAssistance = LegalAssistance
  { ident :: PK Int LegalAssistance ""
  , requestType :: F (Maybe (IdentI RequestType)) "requestType" "Тип запроса"
  , whatToSay1  :: F (Maybe Text) "whatToSay1" "Описание проблемы"
  , activity    :: F (Maybe (IdentI Activity)) "activity" "Тип действия"
  }
  deriving Typeable


instance Model LegalAssistance where
  type TableName LegalAssistance = "kentbl"
  type Parent LegalAssistance = Service
  parentInfo = ExParent modelInfo
  modelInfo = mkModelInfo LegalAssistance ident
  modelView v = case parentView v :: Maybe (ModelView LegalAssistance) of
    Nothing -> Nothing
    Just mv -> Just $ mv {mv_title = "Юридическая помощь"}
