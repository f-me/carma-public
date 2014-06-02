module Carma.Model.Diagnostics.Wazzup where

import Data.Text
import Data.Typeable

import Carma.Model.PgTypes ()
import Data.Model
import Data.Model.View

import Carma.Model.Diagnostics.Cause      (Cause)
import Carma.Model.Diagnostics.Part       (Part)
import Carma.Model.Diagnostics.Suggestion (Suggestion)
import Carma.Model.Diagnostics.System     (System)


data Wazzup = Wazzup
  { ident :: PK Int Wazzup "Что случилось"
  , label
    :: F Text "label" "Описание"
  , system
    :: F (Maybe (IdentI System)) "system" "Система"
  , part
    :: F (Maybe (IdentI Part)) "part" "Узел/деталь"
  , cause
    :: F (Maybe (IdentI Cause)) "cause" "Описание причины неисправности"
  , suggestion
    :: F (Maybe (IdentI Suggestion)) "suggestion" "Рекомендация"
  } deriving Typeable

instance Model Wazzup where
  type TableName Wazzup = "Wazzup"
  modelInfo = mkModelInfo Wazzup ident
  modelView = \case
    "" -> Just $ modifyView defaultView
                 [ setMeta "dictionaryParent" (fieldName system) part ]
    _  -> Nothing
