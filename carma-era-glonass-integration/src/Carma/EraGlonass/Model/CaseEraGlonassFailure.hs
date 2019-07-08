{-# LANGUAGE OverloadedStrings, LambdaCase, DataKinds, TypeFamilies #-}
{-# LANGUAGE DeriveGeneric, ExplicitNamespaces #-}

-- | A model to collect failures of Era Glonass integration calls.
module Carma.EraGlonass.Model.CaseEraGlonassFailure where

import           GHC.Generics (type Generic)

import           Data.Typeable

import           Data.Text (type Text)

import           Data.Model
import           Data.Model.View
import           Data.Aeson

import           Carma.Model.Types ()
import           Carma.Model.PgTypes ()
import           Carma.Monad.Clock (type UTCTime)
import           Carma.EraGlonass.Types.EGRequestId (type EGRequestId)
import           Carma.EraGlonass.Model.CaseEraGlonassFailure.Types


data CaseEraGlonassFailure
   = CaseEraGlonassFailure
   { ident
     :: PK Int CaseEraGlonassFailure
        "Информация об ошибке на точке интеграции с ЭРА-ГЛОНАСС"

   , ctime
       :: F UTCTime "ctime" "Дата-время фиксации ошибки"

   , integrationPoint
       :: F EGIntegrationPoint "integrationPoint" "Точка интеграции"

   , requestId
       :: F (Maybe EGRequestId) "requestId" "Идентификатор запроса на оказание услуги"

   , requestBody
       :: F (Maybe Value) "requestBody" "Содержимое неудачного запроса"

   , responseBody
       :: F (Maybe Value) "responseBody" "Содержимое неудачного ответа"

   , comment
       :: F (Maybe Text) "comment" "Дополнительный комментарий"

   } deriving (Generic, Typeable)


instance Model CaseEraGlonassFailure where
  type TableName CaseEraGlonassFailure = "CaseEraGlonassFailure"
  modelInfo = mkModelInfo CaseEraGlonassFailure ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
