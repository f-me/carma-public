module Carma.Model.SubProgramService where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.ServiceNames hiding (ident)
import Carma.Model.SubProgram.Type hiding (ident, parent)
import Carma.Model.Types (TInt)


data SubProgramService = SubProgramService
    { ident       :: PK Int SubProgramService "Услуга по подпрограмме"
    , parent      :: F (IdentI SubProgram) "parent" "Подпрограмма"
    , sType       :: F (IdentI ServiceNames) "type" "Услуга"
    , maxCost     :: F (Maybe Text)
                     "maxCost"
                     "Лимит стоимости"
    , maxDistance :: F (Maybe TInt)
                     "maxDistance"
                     "Лимит расстояния"
    , maxPeriod   :: F (Maybe TInt)
                     "maxPeriod"
                     "Лимит продолжительности (в днях)"
    , maxCount    :: F (Maybe TInt)
                     "maxCount"
                     "Лимит количества предоставления услуги"
    } deriving Typeable

instance Model SubProgramService where
  type TableName SubProgramService = "SubProgramService"
  modelInfo = mkModelInfo SubProgramService ident
  modelView _ = modifyView defaultView
                [ widget "text" maxDistance
                , widget "text" maxPeriod
                , widget "text" maxCount
                , invisible parent
                ]
