module Carma.Model.BikeTowType where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Types()
import Carma.Model.PgTypes()

data BikeTowType = BikeTowType
  { ident
    :: PK Int BikeTowType "Тип мотоэвакуации"
  , label
    :: F Text "label" "Тип"
  } deriving Typeable

instance Model BikeTowType where
  type TableName BikeTowType = "BikeTowType"
  modelInfo = mkModelInfo BikeTowType ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
