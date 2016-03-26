{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.PartnerDelay.Reason where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH
import Carma.Model.PgTypes()

data PartnerDelay_Reason = PartnerDelay_Reason
  { ident :: PK Int PartnerDelay_Reason ""
  , label :: F Text "label" ""
  } deriving Typeable

mkIdents [t|PartnerDelay_Reason|]
 [ ("other", 1)
 ]

instance Model PartnerDelay_Reason where
  type TableName PartnerDelay_Reason = "PartnerDelay_Reason"
  idents = Carma.Model.PartnerDelay.Reason.idents
  modelInfo = mkModelInfo PartnerDelay_Reason ident
  modelView _ = Just defaultView
