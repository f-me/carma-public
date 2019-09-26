{-# LANGUAGE OverloadedStrings, LambdaCase, DeriveGeneric #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}

module Carma.Model.EraGlonassCaseStatusUpdate where

import           GHC.Generics (Generic)

import           Data.Typeable (Typeable)

import           Data.Model
import           Data.Model.View
import           Data.Time.Clock (UTCTime)
import           Data.Text (Text)

import           Carma.Model.LegacyTypes (Phone)
import           Carma.Model.Case (Case)
import           Carma.Model.CaseStatus (CaseStatus)
import           Carma.Model.Types ()
import           Carma.Model.PgTypes ()


data EraGlonassCaseStatusUpdate
   = EraGlonassCaseStatusUpdate
   { ident
       :: PK Int EraGlonassCaseStatusUpdate
          "Запрос на обновление статуса заявки на обслуживание от ЭРА-ГЛОНАСС"

   , ctime
       :: F UTCTime "ctime" "Дата и время обновления статуса заявки"

   , caseId
       :: F (IdentI Case) "caseId" "Связанный кейс"

   , newCaseStatus
       :: F (IdentI CaseStatus) "newCaseStatus" "Новый статус заявки"

   , isProcessed
       :: F Bool "isProcessed" "Запрос обработан"

   , processTime
       :: F (Maybe UTCTime) "processTime" "Время когда запрос был обработан"

   , customerName
       :: F (Maybe Text) "customerName" "Имя звонившего"

   , customerPhone
       :: F (Maybe Phone) "customerPhone" "Телефон звонившего"

   , terminalPhone
       :: F (Maybe Phone) "terminalPhone" "Номер терминала автомобиля"

   } deriving (Typeable, Generic)


instance Model EraGlonassCaseStatusUpdate where
  type TableName EraGlonassCaseStatusUpdate = "EraGlonassCaseStatusUpdate"
  modelInfo = mkModelInfo EraGlonassCaseStatusUpdate ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
