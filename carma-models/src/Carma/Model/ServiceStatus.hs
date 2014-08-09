{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.ServiceStatus where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.Types()
import Carma.Model.PgTypes()

data ServiceStatus = ServiceStatus
  { ident
    :: PK Int ServiceStatus "Статус услуги"
  , label
    :: F Text "label" "Тип"
  , button
    :: F Bool "button" "Кнопка"
  } deriving Typeable

mkIdents [t|ServiceStatus|]
 [ ("backoffice", 1)
 , ("creating", 2)
 , ("recallClient", 3)
 , ("clientCanceled", 4)
 , ("mechanicConf", 5)
 , ("dealerConf", 6)
 , ("checkNeeded", 7)
 , ("checking", 8)
 , ("mistake", 9)
 , ("makerApproval", 11)
 , ("order", 12)
 , ("needPartner", 13)
 , ("canceled", 14)
 , ("ordered", 15)
 , ("delayed", 16)
 , ("inProgress", 17)
 , ("falseCall", 18)
 , ("ok", 19)
 , ("closed", 20)
 ]

instance Model ServiceStatus where
  type TableName ServiceStatus = "ServiceStatus"
  idents = Carma.Model.ServiceStatus.idents
  modelInfo = mkModelInfo ServiceStatus ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
