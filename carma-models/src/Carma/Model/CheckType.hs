module Carma.Model.CheckType where

import Data.Text
import Data.Typeable
import Data.Vector

import Data.Model
import Data.Model.View

import Carma.Model.Types()

data CheckType = CheckType
  { ident    :: PK Int CheckType "Вид ТО"
  , label    :: F Text           "label" "Класс"
  , synonyms :: F (Vector Text)  "synonyms" "Синонимы"
  } deriving Typeable

instance Model CheckType where
  type TableName CheckType = "CheckType"
  modelInfo = mkModelInfo CheckType ident
  modelView _ = defaultView
