module Carma.Model.SubProgramService
    (SubProgramService(..))

where

import Data.Model
import Data.Model.View

import Carma.Model.SubProgram.Type

instance Model SubProgramService where
  type TableName SubProgramService = "SubProgramService"
  modelInfo = mkModelInfo SubProgramService sIdent
  modelView = \case
    "" -> Just $ modifyView defaultView
                [ widget "text" maxDistance
                , widget "text" maxPeriod
                , widget "text" maxCount
                , invisible sParent
                ]
    _  -> Nothing
