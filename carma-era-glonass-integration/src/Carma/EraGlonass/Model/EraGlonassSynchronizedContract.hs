{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Carma.EraGlonass.Model.EraGlonassSynchronizedContract where

import           Data.Typeable

import           Data.Model
import           Data.Model.View
import           Data.Time.Clock
import           Data.Text

import           Carma.Model.Contract (Contract)
import           Carma.Model.Types ()
import           Carma.Model.PgTypes ()


data EraGlonassSynchronizedContract
   = EraGlonassSynchronizedContract
   { ident
     :: PK Int EraGlonassSynchronizedContract
        "Фиксация синхронизированного контракта с ЭРА-ГЛОНАСС"
   , ctime
     :: F UTCTime "ctime" "Время создания фиксации"
   , contract
     :: F (IdentI Contract) "contractId" "Контракт"
   , vin
     :: F Text "vin" "Первичный VIN"
   , isHandledByCarma
     :: F Bool "isHandledByCarma" "Синхронизирован"
   , lastStatusChangeTime
     :: F (Maybe UTCTime) "lastStatusChangeTime" "Время последней смены статуса"
   } deriving Typeable


instance Model EraGlonassSynchronizedContract where
  type TableName EraGlonassSynchronizedContract =
         "EraGlonassSynchronizedContract"

  modelInfo = mkModelInfo EraGlonassSynchronizedContract ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
