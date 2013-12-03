
module Carma.Model.Service.LegalAssistance where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.LegacyTypes
import Carma.Model.Service (Service)


data LegalAssistance = LegalAssistance
  { ident :: PK Int LegalAssistance ""
  , requestType :: F (IdentT RequestType) "requestType" "Тип запроса"
  , whatToSay1  :: F Text "whatToSay1" "Описание проблемы"
  , activity    :: F (IdentT Activity) "activity" "Тип действия"
  }
  deriving Typeable


instance Model LegalAssistance where
  type TableName LegalAssistance = "kentbl"
  type Parent LegalAssistance = Service
  modelInfo = mkModelInfo LegalAssistance ident
  modelView _ = (defaultView :: ModelView LegalAssistance)
    {mv_title = "Юридическая помощь"}
