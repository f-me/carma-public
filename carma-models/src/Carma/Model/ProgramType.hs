{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.ProgramType where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.Types()
import Carma.Model.PgTypes()

data ProgramType = ProgramType
  { ident                 :: PK Int ProgramType "Тип программы"
  , label                 :: F Text             "label"  "Тип"
  } deriving Typeable

mkIdents [t|ProgramType|]
 [ ("b2b", 1)
 , ("b2c", 2)
 ]

instance Model ProgramType where
  type TableName ProgramType = "ProgramType"
  idents = Carma.Model.ProgramType.idents
  modelInfo = mkModelInfo ProgramType ident
  modelView _ = defaultView
