{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.PartnerDelay.Notified where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH
import Carma.Model.PgTypes()

data PartnerDelay_Notified = PartnerDelay_Notified
  { ident :: PK Int PartnerDelay_Notified ""
  , label :: F Text "label" ""
  } deriving Typeable

mkIdents [t|PartnerDelay_Notified|]
 [ ("yes", 1)
 , ("no", 2)
 ]

instance Model PartnerDelay_Notified where
  type TableName PartnerDelay_Notified = "PartnerDelay_Notified"
  idents = Carma.Model.PartnerDelay.Notified.idents
  modelInfo = mkModelInfo PartnerDelay_Notified ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
