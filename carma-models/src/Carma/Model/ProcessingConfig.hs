{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.ProcessingConfig where

import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.Types()
import Carma.Model.PgTypes()


data ProcessingConfig = ProcessingConfig
  { ident
    :: PK Int ProcessingConfig "Параметры КЦ"
  , actionsFirst
    :: F Bool "actionsFirst" "Действия приоритетнее звонков"
  , acSeconds
    :: F Int "afterCallSeconds" "Время в After call, с"
  , callWaitSeconds
    :: F Int "callWaitSeconds" "Длительность ожидания звонков, с"
  } deriving Typeable


mkIdents [t|ProcessingConfig|]
 [ ("main", 1)
 ]


instance Model ProcessingConfig where
  type TableName ProcessingConfig = "ProcessingConfig"
  idents = Carma.Model.ProcessingConfig.idents
  modelInfo = mkModelInfo ProcessingConfig ident
  modelView = \case
    "" -> Just $ modifyView defaultView
          [ infoText "actionsFirst" actionsFirst
          , infoText "acSeconds" acSeconds
          , infoText "callWaitSeconds" callWaitSeconds
          ]
    _  -> Nothing
