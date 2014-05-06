module Carma.Model.Diagnostics.Part where

import Data.Text
import Data.Typeable

import Carma.Model.PgTypes ()
import Data.Model
import Data.Model.View

import Carma.Model.Diagnostics.System hiding (ident)

data Part = Part
  { ident  :: PK Int Part "Узел/деталь"
  , parent :: F (IdentI System) "parent" "Система"
  , label  :: F Text "label" "Название"
  , fdds   :: F (Maybe Text) "fdds" "FDDS-код"
  } deriving Typeable

instance Model Part where
  type TableName Part = "Part"
  modelInfo = mkModelInfo Part ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
