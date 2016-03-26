{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.PartnerDelay.Exceptional where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH
import Carma.Model.PgTypes()

data PartnerDelay_Exceptional = PartnerDelay_Exceptional
  { ident :: PK Int PartnerDelay_Exceptional ""
  , label :: F Text "label" ""
  } deriving Typeable

mkIdents [t|PartnerDelay_Exceptional|]
 [ ("yes", 1)
 , ("no", 2)
 ]

instance Model PartnerDelay_Exceptional where
  type TableName PartnerDelay_Exceptional = "PartnerDelay_Exceptional"
  idents = Carma.Model.PartnerDelay.Exceptional.idents
  modelInfo = mkModelInfo PartnerDelay_Exceptional ident
  modelView _ = Just defaultView
