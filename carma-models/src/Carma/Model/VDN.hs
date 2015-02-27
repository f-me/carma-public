module Carma.Model.VDN where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Types ()
import Carma.Model.PgTypes ()

import Carma.Model.Program


data VDN = VDN
  { ident
    :: PK Int VDN "Телефонная линия (VDN)"
  , number
    :: F Text "number" "Номер линии"
  , label
    :: F Text "label" "Название"
  , greeting
    :: F Text "greeting" "Приветствие"
  , program
    :: F (Maybe (IdentI Program)) "program" "Программа"
  } deriving Typeable


instance Model VDN where
  type TableName VDN = "VDN"
  modelInfo = mkModelInfo VDN Carma.Model.VDN.ident
  modelView = \case
    "" -> Just $ modifyView defaultView
          [ infoText "VDN Avaya" number]
    _  -> Nothing
