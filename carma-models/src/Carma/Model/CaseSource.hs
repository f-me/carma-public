{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.CaseSource where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.Types()
import Carma.Model.PgTypes()

data CaseSource = CaseSource
  { ident
    :: PK Int CaseSource "Источник кейса"
  , label
    :: F Text "label" "Тип"
  } deriving Typeable

mkIdents [t|CaseSource|]
 [ ("op", 1)
 , ("mobile", 2)
 , ("mobileAccident", 3)
 ]

instance Model CaseSource where
  type TableName CaseSource = "CaseSource"
  idents = Carma.Model.CaseSource.idents
  modelInfo = mkModelInfo CaseSource ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
