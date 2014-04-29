module Carma.Model.Diagnostics.Suggestion where

import Data.Text
import Data.Typeable

import Carma.Model.PgTypes ()
import Data.Model
import Data.Model.View


data Suggestion = Suggestion
  { ident :: PK Int Suggestion "Рекомендация"
  , label :: F Text "label" "Рекомендация"
  , fdds  :: F (Maybe Text) "fdds" "FDDS-код"
  } deriving Typeable

instance Model Suggestion where
  type TableName Suggestion = "Suggestion"
  modelInfo = mkModelInfo Suggestion ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
