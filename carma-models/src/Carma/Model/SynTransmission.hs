module Carma.Model.SynTransmission where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Types()

import Carma.Model.Transmission (Transmission)

data SynTransmission = SynTransmission
  { ident         :: PK Int SynTransmission ""
  , transmission  :: F (IdentI Transmission) "transmission" "Коробка передач"
  , synonym       :: F Text                  "label"  "Синоним"
  } deriving Typeable


instance Model SynTransmission where
  type TableName SynTransmission = "SynTransmission"
  modelInfo = mkModelInfo SynTransmission ident
  modelView _ = modifyView defaultView
                [ required transmission
                , required synonym
                ]
