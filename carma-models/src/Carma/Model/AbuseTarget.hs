module Carma.Model.AbuseTarget where


import Data.Text (Text)
import Data.Typeable

import Carma.Model.Types()
import Carma.Model.PgTypes()

import Data.Model
import Data.Model.View

data AbuseTarget = AbuseTarget
  {ident   :: PK Int AbuseTarget ""
  ,label   :: F Text "label" "Название цвета"
  } deriving Typeable

instance Model AbuseTarget where
  type TableName AbuseTarget = "AbuseTarget"
  modelInfo = mkModelInfo AbuseTarget ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
