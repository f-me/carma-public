module Carma.Model.Service.Bank where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.Types()

import Carma.Model.Activity    (Activity)
import Carma.Model.RequestType (RequestType)
import Carma.Model.Service     (Service)


data Bank = Bank
  { ident :: PK Int Bank ""
  , requestType :: F (Maybe (IdentI RequestType))
                   "requestType" "Тип запроса"
  , whatToSay1  :: F (Maybe Text)
                   "whatToSay1" "Описание проблемы"
  , activity    :: F (Maybe (IdentI Activity))
                   "activity" "Тип действия"
  }
  deriving Typeable


instance Model Bank where
  type TableName Bank = "banktbl"
  type Parent Bank = Service
  parentInfo = ExParent modelInfo
  modelInfo = mkModelInfo Bank ident
  modelView v = case parentView v :: Maybe (ModelView Bank) of
    Nothing -> Nothing
    Just mv -> Just $ mv {mv_title = "Банковская поддержка"}
