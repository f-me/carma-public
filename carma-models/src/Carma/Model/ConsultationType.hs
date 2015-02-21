{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.ConsultationType where

import Data.Text
import Data.Typeable
import Data.Vector

import Data.Model
import Data.Model.View

import Carma.Model.Types()
import Carma.Model.PgTypes()


data ConsultationType = ConsultationType
  { ident :: PK Int ConsultationType "Тип консультации"
  , label :: F Text "label" "Тип"
  } deriving Typeable


instance Model ConsultationType where
  type TableName ConsultationType = "ConsultationType"
  modelInfo = mkModelInfo ConsultationType ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
