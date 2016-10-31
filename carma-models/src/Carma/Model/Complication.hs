module Carma.Model.Complication where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Types()
import Carma.Model.PgTypes()


data Complication = Complication
  { ident :: PK Int Complication "Тип осложнения"
  , label :: F Text "label" "Тип"
  } deriving Typeable


instance Model Complication where
  type TableName Complication = "Complication"
  modelInfo = mkModelInfo Complication ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
