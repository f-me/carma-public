{-# LANGUAGE TemplateHaskell #-}
module Carma.Model.SmsTokenName where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.TH
import Data.Model.View

import Carma.Model.Types()
import Carma.Model.PgTypes()


data SmsTokenName = SmsTokenName
  { ident   :: PK Int SmsTokenName "Переменные шаблонов СМС"
  , label   :: F Text "label" "Описание"
  , varName :: F Text "var_name" "Название переменной"
  }
  deriving Typeable


mkIdents [t|SmsTokenName|]
 [ ("program_contact_info", 1)
 , ("program_from_name", 2)
 , ("program_info", 3)
 ]


instance Model SmsTokenName where
  type TableName SmsTokenName = "SmsTokenName"
  modelInfo = mkModelInfo SmsTokenName ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
