module Carma.Model.VipNumber where

import Data.Text
import Data.Time.Clock
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Types ()
import Carma.Model.PgTypes ()

import Carma.Model.Program


data VipNumber = VipNumber
  { ident
    :: PK Int VipNumber "VIP-номер"
  , number
    :: F Text "number" "Номер"
  } deriving Typeable


instance Model VipNumber where
  type TableName VipNumber = "VipNumber"
  modelInfo = mkModelInfo VipNumber Carma.Model.VipNumber.ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
