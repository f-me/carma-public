{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.ConsultationType where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.Types()
import Carma.Model.PgTypes()


data ConsultationType = ConsultationType
  { ident :: PK Int ConsultationType "Тип консультации"
  , label :: F Text "label" "Тип"
  } deriving Typeable


mkIdents [t|ConsultationType|]
 [ ("oper", 1)
 , ("mech", 2)
 ]


instance Model ConsultationType where
  type TableName ConsultationType = "ConsultationType"
  idents = Carma.Model.ConsultationType.idents
  modelInfo = mkModelInfo ConsultationType ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
