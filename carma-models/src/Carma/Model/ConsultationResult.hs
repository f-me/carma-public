{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.ConsultationResult where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Types()
import Carma.Model.PgTypes()


data ConsultationResult = ConsultationResult
  { ident :: PK Int ConsultationResult "Результат консультации"
  , label :: F Text "label" "Результат"
  } deriving Typeable


instance Model ConsultationResult where
  type TableName ConsultationResult = "ConsultationResult"
  modelInfo = mkModelInfo ConsultationResult ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
