module Carma.Model.Service.Bank where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.Types()
import Carma.Model.LegacyTypes
import Carma.Model.Service (Service)


data Bank = Bank
  { ident :: PK Int Bank ""
  , bill_billNumber
                :: F (Maybe Text) "bill_billNumber" "Номер счёта"
  , bill_billingCost
                :: F (Maybe Int)  "bill_billingCost" "Сумма по счёту"
  , bill_billingDate
                :: F (Maybe LegacyDate)
                   "bill_billingDate" "Дата выставления счёта"
  , requestType :: F (Maybe (IdentT RequestType))
                   "requestType" "Тип запроса"
  , whatToSay1  :: F (Maybe Text)
                   "whatToSay1" "Описание проблемы"
  , activity    :: F (Maybe (IdentT Activity))
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
