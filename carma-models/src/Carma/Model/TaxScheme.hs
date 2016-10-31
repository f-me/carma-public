module Carma.Model.TaxScheme where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.PgTypes()
import Carma.Model.Types()

data TaxScheme = TaxScheme
  { ident
    :: PK Int TaxScheme "Форма налогообложения"
  , label
    :: F Text "label" "Тип"
  } deriving Typeable

instance Model TaxScheme where
  type TableName TaxScheme = "TaxScheme"
  modelInfo = mkModelInfo TaxScheme ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
