{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.CarClass where

import Data.Text
import Data.Typeable
import Data.Vector

import Data.Model
import Data.Model.TH
import Data.Model.View

import Carma.Model.Types()
import Carma.Model.PgTypes()

data CarClass = CarClass
  { ident    :: PK Int CarClass "Класс автомобиля"
  , label    :: F Text          "label" "Класс"
  , synonyms :: F (Maybe (Vector Text)) "synonyms" "Синонимы"
  } deriving Typeable


mkIdents [t|CarClass|]
 [ ("psab", 7)
 , ("psam1", 8)
 , ("psam2", 9)
 , ("psah", 10)
 ]


instance Model CarClass where
  type TableName CarClass = "CarClass"
  modelInfo = mkModelInfo CarClass ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
