{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Carma.EraGlonass.Model.CaseEraGlonassData where

import           Data.Typeable

import           Data.Model
import           Data.Model.View

import           Carma.Model.Case (Case)
import           Carma.Model.Types ()
import           Carma.Model.PgTypes ()


data CaseEraGlonassData
   = CaseEraGlonassData
   { ident
     :: PK Int CaseEraGlonassData
        "Дополнительные данные о Карточке Вызова из ЭРА-ГЛОНАСС"
   , associatedCase
     :: F (IdentI Case) "caseId" "Связанный кейс"
   } deriving Typeable


instance Model CaseEraGlonassData where
  type TableName CaseEraGlonassData = "CaseEraGlonassData"
  modelInfo = mkModelInfo CaseEraGlonassData ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
