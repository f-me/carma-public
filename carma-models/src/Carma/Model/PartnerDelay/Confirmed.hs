{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.PartnerDelay.Confirmed where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH
import Carma.Model.PgTypes()

data PartnerDelay_Confirmed = PartnerDelay_Confirmed
  { ident :: PK Int PartnerDelay_Confirmed ""
  , label :: F Text "label" ""
  } deriving Typeable

mkIdents [t|PartnerDelay_Confirmed|]
 [ ("yes", 1)
 , ("no", 2)
 , ("needConfirmation", 3)
 ]

instance Model PartnerDelay_Confirmed where
  type TableName PartnerDelay_Confirmed = "PartnerDelay_Confirmed"
  idents = Carma.Model.PartnerDelay.Confirmed.idents
  modelInfo = mkModelInfo PartnerDelay_Confirmed ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
