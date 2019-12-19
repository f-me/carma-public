{-# LANGUAGE DeriveGeneric, GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.Action.Persistent where

import           GHC.Generics (Generic)

import           Data.Typeable
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)

import           Database.Persist.TH

-- TODO FIXME Write @Persistent@ instances for @HMDiffTime@.
-- import           Carma.Model.Types (HMDiffTime)

import           Carma.Model.Call.Persistent (CallId)
import           Carma.Model.Case.Persistent (CaseId)
import           Carma.Model.Service.Persistent (ServiceId)
import           Carma.Model.Usermeta.Persistent (UsermetaId)
import           Carma.Model.ActionResult.Persistent (ActionResultId)
import           Carma.Model.ActionType.Persistent (ActionTypeId)
import           Carma.Model.Role.Persistent (RoleId)


-- | @Action@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Action sql=actiontbl
  callId      CallId         Maybe sql=callid
  caseId      CaseId         Maybe sql=caseid
  serviceId   ServiceId      Maybe sql=serviceid
  aType       ActionTypeId         sql=type
  duetime     UTCTime              sql=duetime
  comment     Text           Maybe sql=comment
  redirectTo  UsermetaId     Maybe sql=redirectto

  -- TODO FIXME Write @Persistent@ instances for @HMDiffTime@.
  -- deferBy     HMDiffTime     Maybe sql=deferby

  result      ActionResultId Maybe sql=result
  ctime       UTCTime              sql=ctime
  assignTime  UTCTime        Maybe sql=assigntime
  openTime    UTCTime        Maybe sql=opentime
  closeTime   UTCTime        Maybe sql=closetime
  assignedTo  UsermetaId     Maybe sql=assignedto
  targetGroup RoleId               sql=targetgroup
  parent      ActionId       Maybe sql=parent

  deriving Generic Typeable Show
|]
