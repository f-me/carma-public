{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.ActionType where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.Types (TInt)
import Carma.Model.PgTypes()

data ActionType = ActionType
  { ident
    :: PK Int ActionType "Действие"
  , label
    :: F Text "label" "Тип действия"
  , desc
    :: F Text "desc" "Описание"
  , priority
    :: F TInt "priority" "Приоритет"
  } deriving Typeable

mkIdents [t|ActionType|]
 [ ("orderService", 1)
 , ("orderServiceAnalyst", 2)
 , ("tellClient", 3)
 , ("checkStatus", 4)
 , ("needPartner", 5)
 , ("checkEndOfService", 6)
 , ("closeCase", 7)
 , ("getDealerInfo", 8)
 , ("cancelService", 9)
 , ("makerApproval", 10)
 , ("tellMakerDeclined", 11)
 , ("addBill", 12)
 , ("billmanNeedInfo", 13)
 , ("headCheck", 14)
 , ("directorCheck", 15)
 , ("accountCheck", 16)
 , ("analystCheck", 17)
 , ("complaintResolution", 18)
 ]

instance Model ActionType where
  type TableName ActionType = "ActionType"
  idents = Carma.Model.ActionType.idents
  modelInfo = mkModelInfo ActionType ident
  modelView = \case
    "" -> Just $ modifyView defaultView $
          [ infoText "actpriority" priority
          ]
    _  -> Nothing
