module Carma.Model.SubProgramContact
    (SubProgramContact(..))

where

import Data.Model
import Data.Model.View

import Carma.Model.SubProgram.Type

instance Model SubProgramContact where
  type TableName SubProgramContact = "SubProgramContact"
  modelInfo = mkModelInfo SubProgramContact cIdent
  modelView _ = modifyView defaultView
                [ invisible cParent
                , setMeta "regexp" "email" email
                , setMeta "regexp" "phone" phone
                ]
