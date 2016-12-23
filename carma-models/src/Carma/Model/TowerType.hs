{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.TowerType where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.Types()
import Carma.Model.PgTypes()


data TowerType = TowerType
  { ident :: PK Int TowerType "Тип эвакуатора"
  , label :: F Text "label" "Тип"
  } deriving Typeable


mkIdents [t|TowerType|]
 [ ("evac", 1)
 , ("manip", 2)
 , ("carts", 3)
 , ("heavy", 4)
 , ("longcat", 5)
 ]


instance Model TowerType where
  type TableName TowerType = "TowerType"
  idents = Carma.Model.TowerType.idents
  modelInfo = mkModelInfo TowerType ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
