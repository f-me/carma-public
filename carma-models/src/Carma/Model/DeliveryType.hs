module Carma.Model.DeliveryType where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Types()
import Carma.Model.PgTypes()

data DeliveryType = DeliveryType
  { ident
    :: PK Int DeliveryType "Тип доставки"
  , label
    :: F Text "label" "Тип"
  } deriving Typeable

instance Model DeliveryType where
  type TableName DeliveryType = "DeliveryType"
  modelInfo = mkModelInfo DeliveryType ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
