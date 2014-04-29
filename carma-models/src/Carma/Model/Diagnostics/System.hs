module Carma.Model.Diagnostics.System where

import Data.Text
import Data.Typeable

import Carma.Model.PgTypes ()
import Carma.Model.Types (TInt)
import Data.Model
import Data.Model.View


data System = System
  { ident :: PK Int System "Система"
  , label :: F Text "label" "Название"
  , fdds  :: F (Maybe Text) "fdds" "FDDS-код"
  } deriving Typeable

instance Model System where
  type TableName System = "System"
  modelInfo = mkModelInfo System ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
