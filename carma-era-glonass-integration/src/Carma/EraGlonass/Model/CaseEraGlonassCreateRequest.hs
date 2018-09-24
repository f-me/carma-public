{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Carma.EraGlonass.Model.CaseEraGlonassCreateRequest where

import           Data.Typeable

import           Data.Model
import           Data.Model.View
import           Data.Time.Clock
import           Data.Text

import           Carma.Model.Case (Case)
import           Carma.Model.Types ()
import           Carma.Model.PgTypes ()
import           Carma.EraGlonass.Types.RequestId (RequestId)
import           Carma.EraGlonass.Types.EGCallCardId (EGCallCardId)
import           Carma.EraGlonass.Types.EGCreateCallCardRequest
                   (EGCreateCallCardRequest)


data CaseEraGlonassCreateRequest
   = CaseEraGlonassCreateRequest
   { ident
     :: PK Int CaseEraGlonassCreateRequest
        "Дополнительные данные о поступлении Карточки Вызова из ЭРА-ГЛОНАСС"
   , ctime
     :: F UTCTime "ctime" "Дата поступления Карточки Вызова"
   , associatedCase
     :: F (IdentI Case) "caseId" "Связанный кейс"
   , requestId
     :: F RequestId
          "requestId"
          "Идентификатор запроса поступления Карточки Вызова"
   , callCardId
     :: F EGCallCardId "callCardId" "Идентификатор Карточки Вызова"
   , responseId
     :: F Text "responseId" "Идентификатор ответа, отданный ЭГ"
   , requestBody
     :: F EGCreateCallCardRequest
          "requestBody"
          "Полученные данные при поступлении Карточки Вызова"
   } deriving Typeable


instance Model CaseEraGlonassCreateRequest where
  type TableName CaseEraGlonassCreateRequest = "CaseEraGlonassCreateRequest"
  modelInfo = mkModelInfo CaseEraGlonassCreateRequest ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
