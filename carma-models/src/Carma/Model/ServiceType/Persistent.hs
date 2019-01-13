{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.ServiceType.Persistent where

import           Data.Text (Text)
import           Data.Typeable

import           Database.Persist.TH
import           Database.Persist.Sql (toSqlKey)

import           Carma.Model.CtrModel.Persistent (CtrModelId)


-- | @ServiceType@ persistent model.
mkPersist sqlSettings [persistLowerCase|
ServiceType sql=ServiceType
  label    Text       sql=label
  icon     Text       sql=icon
  fdds     Text       sql=fdds
  model    CtrModelId sql=model
  ordering Int        sql=ordering

  deriving Typeable Show
|]


-- | @ServiceType@ predefined IDs.
tech, towage, rent, hotel, taxi, sober, transportation :: ServiceTypeId
deliverCar, deliverParts, ken, tech1, information, consultation :: ServiceTypeId
tickets, tripOn, medic, deliverClient, adjuster, bikeTowage :: ServiceTypeId
tech = toSqlKey 1
towage = toSqlKey 2
rent = toSqlKey 3
hotel = toSqlKey 4
taxi = toSqlKey 5
sober = toSqlKey 6
transportation = toSqlKey 7
deliverCar = toSqlKey 8
deliverParts = toSqlKey 9
ken = toSqlKey 10
tech1 = toSqlKey 11
information = toSqlKey 12
consultation = toSqlKey 13
tickets = toSqlKey 14
tripOn = toSqlKey 15
medic = toSqlKey 16
deliverClient = toSqlKey 17
adjuster = toSqlKey 18
bikeTowage = toSqlKey 19
