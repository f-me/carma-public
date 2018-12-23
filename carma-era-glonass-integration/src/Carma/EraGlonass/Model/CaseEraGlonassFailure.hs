{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- | A model to collect failures of Era Glonass integration calls.
module Carma.EraGlonass.Model.CaseEraGlonassFailure where

import           Data.Typeable

import           Data.Text (Text)

import           Data.Model
import           Data.Model.View
import           Data.Aeson

import           Carma.Model.Types ()
import           Carma.Model.PgTypes ()
import           Carma.EraGlonass.Model.CaseEraGlonassFailure.Types


data CaseEraGlonassFailure
   = CaseEraGlonassFailure
   { ident
     :: PK Int CaseEraGlonassFailure
        "Информация об ошибке вызова точки интеграции с ЭРА-ГЛОНАСС"
   , integrationPoint
     :: F EGIntegrationPoint "integrationPoint" "Точка интеграции"

   , requestBody
     :: F (Maybe Value) "requestBody" "Содержимое неудачного запроса"
   -- , responseBody -- TODO
   --   :: F (Maybe Value) "responseBody" "Содержимое неудачного ответа"

   , comment
     :: F (Maybe Text) "comment" "Дополнительный комментарий"

   , responseId
     :: F (Maybe Text) "responseId" "Идентификатор ответа"
   -- , requestId -- TODO
   --   :: F (Maybe Text) "requestId" "Идентификатор запроса"
   } deriving Typeable


instance Model CaseEraGlonassFailure where
  type TableName CaseEraGlonassFailure = "CaseEraGlonassFailure"
  modelInfo = mkModelInfo CaseEraGlonassFailure ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
