{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.PaymentType where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.Types()
import Carma.Model.PgTypes()

data PaymentType = PaymentType
  { ident
    :: PK Int PaymentType "Тип оплаты"
  , label
    :: F Text "label" "Тип"
  } deriving Typeable

mkIdents [t|PaymentType|]
 [ ("ruamc", 1)
 , ("client", 2)
 , ("mixed", 3)
 , ("refund", 4)
 ]

instance Model PaymentType where
  type TableName PaymentType = "PaymentType"
  idents = Carma.Model.PaymentType.idents
  modelInfo = mkModelInfo PaymentType ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
