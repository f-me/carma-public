module Carma.Model.PartnerService where

import Data.Scientific
import Data.Model
import Data.Typeable

import Data.Model.View
import Carma.Model.Types()
import Carma.Model.Partner (Partner)
import Carma.Model.ServiceType (ServiceType)

data PartnerService = PartnerService
  {ident       :: PK Int PartnerService  ""
  ,parentId    :: F (IdentI Partner)     "parentId"    "Партнёр"
  ,priority1   :: F (Maybe Int)          "priority1"   "Приоритет за нал."
  ,priority2   :: F (Maybe Int)          "priority2"   "Приоритет по безналу город"
  ,priority3   :: F (Maybe Int)          "priority3"   "Приоритет по безналу за город"
  ,serviceName :: F (IdentI ServiceType) "serviceName" "Услуга"
  ,fcp         :: F (Maybe Scientific)   "falseCallPercent" "Процент за ложный вызов"
  } deriving Typeable


instance Model PartnerService where
  type TableName PartnerService = "PartnerService"
  modelInfo = mkModelInfo PartnerService ident
  modelView = \case
    "" -> Just $ modifyView defaultView
      [ invisible parentId
      ]
    _  -> Nothing

