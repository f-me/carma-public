module Carma.Model.Service.Consultation where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.LegacyTypes
import Carma.Model.Service (Service)


data Consultation = Consultation
  { ident       :: PK Int Consultation ""
  , consType    :: F (IdentT ConsultationType) "consType" "Тип консультации"
  , whatToSay1  :: F Text "whatToSay1" "Описание проблемы"
  , orderNumber :: F (Maybe Text) "orderNumber" "Номер заказ-наряда"
  }
  deriving Typeable

instance Model Consultation where
  type TableName Consultation = "consultationtbl"
  type Parent Consultation = Service
  modelInfo = mkModelInfo Consultation ident `withLegacyName` "consultation"
  modelView v = case parentView v :: Maybe (ModelView Consultation) of
    Nothing -> Nothing
    Just mv -> Just $ mv {mv_title = "Консультация"}
