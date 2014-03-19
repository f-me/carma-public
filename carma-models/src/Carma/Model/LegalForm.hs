module Carma.Model.LegalForm where

import Data.Text
import Data.Typeable
import Data.Vector

import Data.Model
import Data.Model.View

import Carma.Model.Types()
import Carma.Model.PgTypes()

data LegalForm = LegalForm
  { ident    :: PK Int LegalForm   "Форма организации"
  , label    :: F Text             "label" "Форма"
  , synonyms :: F (Maybe (Vector Text)) "synonyms" "Синонимы"
  } deriving Typeable

instance Model LegalForm where
  type TableName LegalForm = "LegalForm"
  modelInfo = mkModelInfo LegalForm ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
