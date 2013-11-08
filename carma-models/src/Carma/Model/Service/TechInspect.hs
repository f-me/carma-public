
module Carma.Model.Service.TechInspect where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.Service (Service)


data TechInspect = TechInspect
  { ident       :: PK Int TechInspect
  , requestType :: F Text "requestType" "Тип запроса"
  , whatToSay1  :: F Text {-textarea-} "whatToSay1" "Описание проблемы"
  , activity    :: F Text {-dictionary-} "activity" "Тип действия"
  }
  deriving Typeable

instance Model TechInspect where
  type TableName TechInspect = "tech1"
  type Parent TechInspect = Service
  modelInfo = mkModelInfo TechInspect ident
  modelView _ = defaultView

