{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.TowType where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.Types()
import Carma.Model.PgTypes()

data TowType = TowType
  { ident
    :: PK Int TowType "Тип эвакуации"
  , label
    :: F Text "label" "Тип"
  } deriving Typeable

mkIdents [t|TowType|]
 [ ("dealer", 1)
 ]

instance Model TowType where
  type TableName TowType = "TowType"
  idents = Carma.Model.TowType.idents
  modelInfo = mkModelInfo TowType ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
