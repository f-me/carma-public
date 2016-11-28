{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.ContractCheckStatus where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.Types()
import Carma.Model.PgTypes()

data ContractCheckStatus = ContractCheckStatus
  { ident
    :: PK Int ContractCheckStatus "Статус проверки контракта"
  , label
    :: F Text "label" "Статус"
  } deriving Typeable

mkIdents [t|ContractCheckStatus|]
 [ ("base", 1)
 , ("vinExpired", 7)
 ]

instance Model ContractCheckStatus where
  type TableName ContractCheckStatus = "ContractCheckStatus"
  idents = Carma.Model.ContractCheckStatus.idents
  modelInfo = mkModelInfo ContractCheckStatus ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
