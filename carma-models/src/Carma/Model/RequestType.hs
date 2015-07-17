module Carma.Model.RequestType where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Types()
import Carma.Model.PgTypes()

data RequestType = RequestType
  { ident
    :: PK Int RequestType "Тип запроса"
  , label
    :: F Text "label" "Тип"
  } deriving Typeable

instance Model RequestType where
  type TableName RequestType = "RequestType"
  modelInfo = mkModelInfo RequestType ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
