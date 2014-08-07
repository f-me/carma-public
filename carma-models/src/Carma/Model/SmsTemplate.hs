{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.SmsTemplate where

import Data.Text (Text)
import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.Types()
import Carma.Model.PgTypes()


data SmsTemplate = SmsTemplate
  {ident    :: PK Int SmsTemplate ""
  ,label    :: F Text "label"    "Название шаблона"
  ,isActive :: F Bool "isActive" "Активный?"
  ,text     :: F Text "text"     "Текст шаблона"
  }
  deriving Typeable


mkIdents [t|SmsTemplate|]
 [ ("order", 1)
 , ("cancel", 2)
 , ("complete", 3)
 , ("create", 13)
 , ("parguy", 7)
 ]


instance Model SmsTemplate where
  type TableName SmsTemplate = "SmsTemplate"
  idents = Carma.Model.SmsTemplate.idents
  modelInfo = mkModelInfo SmsTemplate ident
  modelView = \case
    "" -> Just $ modifyView defaultView [textarea text]
    _  -> Nothing
