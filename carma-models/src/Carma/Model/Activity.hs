module Carma.Model.Activity where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Types()
import Carma.Model.PgTypes()

data Activity = Activity
  { ident
    :: PK Int Activity "Тип действия по услуге"
  , label
    :: F Text "label" "Тип"
  } deriving Typeable

instance Model Activity where
  type TableName Activity = "Activity"
  modelInfo = mkModelInfo Activity ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
