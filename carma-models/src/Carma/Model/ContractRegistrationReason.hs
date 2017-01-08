
module Carma.Model.ContractRegistrationReason where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Types()
import Carma.Model.PgTypes()


data ContractRegistrationReason = ContractRegistrationReason
  { ident :: PK Int ContractRegistrationReason
    "Основание для регистрации в программе"
  , label :: F Text "label"
    "Основание для регистрации в программе"
  } deriving Typeable


instance Model ContractRegistrationReason where
  type TableName ContractRegistrationReason = "ContractRegistrationReason"
  modelInfo = mkModelInfo ContractRegistrationReason ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
