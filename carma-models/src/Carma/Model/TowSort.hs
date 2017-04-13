module Carma.Model.TowSort where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Types()
import Carma.Model.PgTypes()

data TowSort = TowSort
  { ident
    :: PK Int TowSort "Тип эвакуации"
  , label
    :: F Text "label" "Тип"
  } deriving Typeable

instance Model TowSort where
  type TableName TowSort = "TowSort"
  modelInfo = mkModelInfo TowSort ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
