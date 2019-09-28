{-# LANGUAGE OverloadedStrings, LambdaCase, DeriveGeneric #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}

module Carma.EraGlonass.Model.CaseEraGlonassCreateRequest where

import           GHC.Generics (Generic)

import           Data.Typeable
import           Data.Aeson (Value)

import           Data.Model
import           Data.Model.View
import           Data.Time.Clock

import           Carma.Model.Case (Case)
import           Carma.Model.Types ()
import           Carma.Model.PgTypes ()
import           Carma.EraGlonass.Types.EGRequestId (EGRequestId)


data CaseEraGlonassCreateRequest
   = CaseEraGlonassCreateRequest
   { ident
       :: PK Int CaseEraGlonassCreateRequest
          "Фиксация поступления запроса на оказание услуги от ЭРА-ГЛОНАСС"

   , ctime
       :: F UTCTime "ctime" "Дата поступления запроса на оказание услуги"

   , associatedCase
       :: F (IdentI Case) "caseId" "Связанный кейс"

   , requestId
       :: F EGRequestId "requestId" "Идентификатор запроса на оказание услуги"

   , requestBody
       :: F Value
            "requestBody"
            "Полученные данные при поступлении запроса на оказание услуги"

   } deriving (Typeable, Generic)


instance Model CaseEraGlonassCreateRequest where
  type TableName CaseEraGlonassCreateRequest = "CaseEraGlonassCreateRequest"
  modelInfo = mkModelInfo CaseEraGlonassCreateRequest ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
