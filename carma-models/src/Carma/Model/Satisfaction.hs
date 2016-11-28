{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.Satisfaction where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.Types()
import Carma.Model.PgTypes()

data Satisfaction = Satisfaction
  { ident
    :: PK Int Satisfaction "Удовлетворённость клиента"
  , label
    :: F Text "label" "Значение"
  } deriving Typeable

mkIdents [t|Satisfaction|]
 [ ("ok", 1)
 , ("none", 2)
 , ("zedIsDead", 3)
 ]

instance Model Satisfaction where
  type TableName Satisfaction = "Satisfaction"
  idents = Carma.Model.Satisfaction.idents
  modelInfo = mkModelInfo Satisfaction ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
