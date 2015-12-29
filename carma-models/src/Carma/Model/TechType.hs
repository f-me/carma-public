{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.TechType where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.Types()
import Carma.Model.PgTypes()

data TechType = TechType
  { ident
    :: PK Int TechType "Тип техпомощи"
  , label
    :: F Text "label" "Тип"
  } deriving Typeable

mkIdents [t|TechType|]
 [ ("charge", 4)
 , ("chargeRuamc", 18)
 , ("starter", 5)
 , ("ac", 6)
 , ("lights", 13)
 , ("hack", 9)
 , ("hackByRuamc", 20)
 , ("wheel", 3)
 , ("wheelTower", 5)
 , ("wheelRuamc", 21)
 , ("fuel", 1)
 , ("fuelTower", 16)
 , ("fuelRuamc", 17)
 ]

instance Model TechType where
  type TableName TechType = "TechType"
  idents = Carma.Model.TechType.idents
  modelInfo = mkModelInfo TechType ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
