module Carma.Model.LegalForm where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Types()

data LegalForm = LegalForm
  { ident    :: PK Int LegalForm   "Форма организации"
  , label    :: F Text             "label" "Форма"
  } deriving Typeable

instance Model LegalForm where
  type TableName LegalForm = "LegalForm"
  modelInfo = mkModelInfo LegalForm ident
  modelView _ = defaultView
