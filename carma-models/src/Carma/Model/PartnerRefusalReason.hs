module Carma.Model.PartnerRefusalReason where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Types()
import Carma.Model.PgTypes()

data PartnerRefusalReason = PartnerRefusalReason
  { ident
    :: PK Int PartnerRefusalReason "Причина отказа партнёра"
  , label
    :: F Text "label" "Причина"
  } deriving Typeable

instance Model PartnerRefusalReason where
  type TableName PartnerRefusalReason = "PartnerRefusalReason"
  modelInfo = mkModelInfo PartnerRefusalReason ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
