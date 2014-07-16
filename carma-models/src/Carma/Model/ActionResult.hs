{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.ActionResult where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.Types()
import Carma.Model.PgTypes()

data ActionResult = ActionResult
  { ident
    :: PK Int ActionResult "Результат действия"
  , label
    :: F Text "label" "Название результата"
  } deriving Typeable

mkIdents [t|ActionResult|]
 [ ("serviceOrdered", 1)
 , ("serviceOrderedSMS", 2)
 , ("needPartner", 3)
 , ("clientCanceledService", 4)
 , ("serviceOrderedAnalyst", 5)
 , ("defer", 6)
 , ("clientOk", 7)
 , ("serviceInProgress", 8)
 , ("partnerFound", 9)
 , ("serviceDone", 10)
 , ("caseClosed", 11)
 , ("gotInfo", 12)
 , ("makerApproved", 13)
 , ("makerDeclined", 14)
 , ("clientNotified", 15)
 , ("billAttached", 16)
 , ("returnToBack", 17)
 , ("returnToBillman", 18)
 ]

instance Model ActionResult where
  type TableName ActionResult = "ActionResult"
  idents = Carma.Model.ActionResult.idents
  modelInfo = mkModelInfo ActionResult ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
