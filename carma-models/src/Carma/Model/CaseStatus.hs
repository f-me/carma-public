{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.CaseStatus where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.Types()
import Carma.Model.PgTypes()

data CaseStatus = CaseStatus
  { ident
    :: PK Int CaseStatus "Статус кейса"
  , label
    :: F Text "label" "Тип"
  , button
    :: F Bool "button" "Кнопка"
  } deriving Typeable

mkIdents [t|CaseStatus|]
 [ ("front", 1)
 , ("needInfo", 2)
 , ("back", 3)
 , ("closed", 4)
 , ("canceled", 5)
 , ("mobileOrder", 6)
 ]

instance Model CaseStatus where
  type TableName CaseStatus = "CaseStatus"
  idents = Carma.Model.CaseStatus.idents
  modelInfo = mkModelInfo CaseStatus ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
