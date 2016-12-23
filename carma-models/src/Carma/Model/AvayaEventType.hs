{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.AvayaEventType where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.TH
import Data.Model.View

import Carma.Model.Types ()
import Carma.Model.PgTypes ()


data AvayaEventType = AvayaEventType
  { ident :: PK Int AvayaEventType "Тип коммуникации в AVAYA"
  , label :: F Text "label" "Название"
  } deriving Typeable


mkIdents [t|AvayaEventType|]
 [ ("incoming", 1)
 , ("out", 2)
 , ("hold", 3)
 , ("unhold", 4)
 , ("conf", 5)
 , ("transfer", 6)
 , ("hangup", 7)
 ]


instance Model AvayaEventType where
  type TableName AvayaEventType = "AvayaEventType"
  idents = Carma.Model.AvayaEventType.idents
  modelInfo = mkModelInfo AvayaEventType ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
