module Carma.Model.SynCarMake where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Types()

import Carma.Model.CarMake (CarMake)

data SynCarMake = SynCarMake
  { ident   :: PK Int SynCarMake ""
  , make    :: F (IdentI CarMake) "make" "Марка"
  , synonym :: F Text             "label"  "Синоним"
  } deriving Typeable


instance Model SynCarMake where
  type TableName SynCarMake = "SynCarMake"
  modelInfo = mkModelInfo SynCarMake ident
  modelView _ = modifyView defaultView
                [ required make
                , required synonym
                ]
