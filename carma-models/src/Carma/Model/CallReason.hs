{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.CallReason where


import Data.Text (Text)
import Data.Typeable
import Data.Model.TH

import Carma.Model.Types()
import Carma.Model.PgTypes()

import Data.Model
import Data.Model.View

import Carma.Model.CallerType (CallerType)

data CallReason = CallReason
  {ident   :: PK Int CallReason ""
  ,label   :: F Text "label" "Название цвета"
  ,parent  :: F (IdentI CallerType) "parent" ""
  } deriving Typeable

mkIdents [t|CallReason|]
 [ ("client_checkParticipate", 1)
 , ("client_contactDealer", 2)
 , ("client_hotline", 3)
 , ("client_complaint", 4)
 , ("client_other", 5)

 , ("partner_closeTicket", 6)
 , ("partner_accounting", 7)
 , ("partner_manager", 8)
 , ("partner_complaint", 9)
 , ("partner_other", 10)

 , ("dealer_analyst", 11)
 , ("dealer_manager", 12)
 , ("dealer_complaint", 13)
 , ("dealer_other", 14)

 , ("employee_other", 15)
 , ("other_other", 16)
 ]

instance Model CallReason where
  type TableName CallReason = "CallReason"
  idents = Carma.Model.CallReason.idents
  modelInfo = mkModelInfo CallReason ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
