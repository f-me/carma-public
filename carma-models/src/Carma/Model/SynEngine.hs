module Carma.Model.SynEngine where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Types()

import Carma.Model.Engine (Engine)

data SynEngine = SynEngine
  { ident   :: PK Int SynEngine ""
  , engine  :: F (IdentI Engine) "engine" "Тип двигателя"
  , synonym :: F Text            "label"  "Синоним"
  } deriving Typeable


instance Model SynEngine where
  type TableName SynEngine = "SynEngine"
  modelInfo = mkModelInfo SynEngine ident
  modelView _ = modifyView defaultView
                [ required engine
                , required synonym
                ]
