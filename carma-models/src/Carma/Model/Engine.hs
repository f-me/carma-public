{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.Engine where

import Data.Text
import Data.Typeable
import Data.Vector

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.Types()
import Carma.Model.PgTypes()

data Engine = Engine
  { ident    :: PK Int Engine   "Тип двигателя"
  , label    :: F Text          "label" "Тип"
  , synonyms :: F (Maybe (Vector Text)) "synonyms" "Синонимы"
  } deriving Typeable

mkIdents [t|Engine|]
 [ ("petrol", 1)
 , ("diesel", 2)
 ]

instance Model Engine where
  type TableName Engine = "Engine"
  modelInfo = mkModelInfo Engine ident
  modelView _ = defaultView
