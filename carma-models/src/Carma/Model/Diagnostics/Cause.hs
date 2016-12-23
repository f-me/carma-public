module Carma.Model.Diagnostics.Cause where

import Data.Text
import Data.Typeable

import Carma.Model.PgTypes ()
import Data.Model
import Data.Model.View


data Cause = Cause
  { ident :: PK Int Cause "Причина неисправности"
  , label :: F Text "label" "Описание"
  , fdds  :: F (Maybe Text) "fdds" "FDDS-код"
  } deriving Typeable

instance Model Cause where
  type TableName Cause = "Cause"
  modelInfo = mkModelInfo Cause ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
