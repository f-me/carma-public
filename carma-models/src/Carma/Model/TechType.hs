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
 , ("starter", 5)
 , ("ac", 6)
 ]

instance Model TechType where
  type TableName TechType = "TechType"
  idents = Carma.Model.TechType.idents
  modelInfo = mkModelInfo TechType ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
