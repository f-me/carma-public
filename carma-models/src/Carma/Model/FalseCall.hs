{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.FalseCall where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.Types()
import Carma.Model.PgTypes()

data FalseCall = FalseCall
  { ident
    :: PK Int FalseCall "Ложный вызов"
  , label
    :: F Text "label" "Тип"
  } deriving Typeable

mkIdents [t|FalseCall|]
 [ ("none", 1)
 , ("bill", 2)
 , ("nobill", 3)
 ]

instance Model FalseCall where
  type TableName FalseCall = "FalseCall"
  idents = Carma.Model.FalseCall.idents
  modelInfo = mkModelInfo FalseCall ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
