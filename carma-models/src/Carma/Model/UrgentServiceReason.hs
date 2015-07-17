{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.UrgentServiceReason where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.TH
import Data.Model.View

import Carma.Model.Types()
import Carma.Model.PgTypes()

data UrgentServiceReason = UrgentServiceReason
  { ident
    :: PK Int UrgentServiceReason "Причина приоритетности услуги"
  , label
    :: F Text "label" "Причина"
  } deriving Typeable

mkIdents [t|UrgentServiceReason|]
 [ ("notUrgent", 1)
 ]

instance Model UrgentServiceReason where
  type TableName UrgentServiceReason = "UrgentServiceReason"
  idents = Carma.Model.UrgentServiceReason.idents
  modelInfo = mkModelInfo UrgentServiceReason ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
