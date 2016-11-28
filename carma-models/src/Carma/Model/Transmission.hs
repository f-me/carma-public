module Carma.Model.Transmission where

import Data.Text
import Data.Typeable
import Data.Vector

import Data.Model
import Data.Model.View

import Carma.Model.Types()
import Carma.Model.PgTypes()

data Transmission = Transmission
  { ident    :: PK Int Transmission "Коробка передач"
  , label    :: F Text "label" "Тип"
  , synonyms :: F (Maybe (Vector Text)) "synonyms" "Синонимы"
  } deriving Typeable

instance Model Transmission where
  type TableName Transmission = "Transmission"
  modelInfo = mkModelInfo Transmission ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
