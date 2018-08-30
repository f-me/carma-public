{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- | A model to collect failures of Era Glonass integration calls.
module Carma.EraGlonass.Model.CaseEraGlonassFailures where

import           Data.Typeable

import           Data.Text (Text)

import           Data.Model
import           Data.Model.View
import           Data.Aeson

import           Carma.Model.Types ()
import           Carma.Model.PgTypes ()
import           Carma.EraGlonass.Model.CaseEraGlonassFailures.Types


data CaseEraGlonassFailures
   = CaseEraGlonassFailures
   { ident
     :: PK Int CaseEraGlonassFailures
        "Информация об ошибке вызова точки интеграции с ЭРА-ГЛОНАСС"
   , integrationPoint
     :: F EGIntegrationPoint "integrationPoint" "Точка интеграции"
   , requestBody
     :: F (Maybe Value) "requestBody" "Содержимое неудачного запроса"
   , comment
     :: F (Maybe Text) "comment" "Дополнительный комментарий"
   } deriving Typeable


instance Model CaseEraGlonassFailures where
  type TableName CaseEraGlonassFailures = "CaseEraGlonassFailures"
  modelInfo = mkModelInfo CaseEraGlonassFailures ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
