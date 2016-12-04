{-# OPTIONS_GHC -fno-warn-orphans #-}

module Carma.Model.SubProgramContact
    (SubProgramContact(..))

where

import Data.Model
import Data.Model.View

import Carma.Model.SubProgram.Type

instance Model SubProgramContact where
  type TableName SubProgramContact = "SubProgramContact"
  modelInfo = mkModelInfo SubProgramContact cIdent
  modelView = \case
    "" -> Just $ modifyView defaultView
                [ invisible cParent
                , regexp regexpEmail email
                , regexp regexpPhone phone
                ]
    _  -> Nothing
