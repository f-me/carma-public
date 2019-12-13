{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.ActionResult.Persistent where

import           Data.Typeable
import           Data.Text (Text)

import           Database.Persist.TH
import           Database.Persist.Sql (toSqlKey)


-- | @ActionResult@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ActionResult sql=ActionResult
  label Text sql=label

  deriving Typeable Show
|]


-- | @ActionResult@ predefined IDs.

serviceOrdered           :: ActionResultId ; serviceOrdered = toSqlKey 1
serviceOrderedSMS        :: ActionResultId ; serviceOrderedSMS = toSqlKey 2
needPartner              :: ActionResultId ; needPartner = toSqlKey 3
clientCanceledService    :: ActionResultId ; clientCanceledService = toSqlKey 4
serviceOrderedAnalyst    :: ActionResultId ; serviceOrderedAnalyst = toSqlKey 5
defer                    :: ActionResultId ; defer = toSqlKey 6
clientOk                 :: ActionResultId ; clientOk = toSqlKey 7
serviceInProgress        :: ActionResultId ; serviceInProgress = toSqlKey 8
partnerFound             :: ActionResultId ; partnerFound = toSqlKey 9
serviceDone              :: ActionResultId ; serviceDone = toSqlKey 10
caseClosed               :: ActionResultId ; caseClosed = toSqlKey 11
gotInfo                  :: ActionResultId ; gotInfo = toSqlKey 12
makerApproved            :: ActionResultId ; makerApproved = toSqlKey 13
makerDeclined            :: ActionResultId ; makerDeclined = toSqlKey 14
clientNotified           :: ActionResultId ; clientNotified = toSqlKey 15
billAttached             :: ActionResultId ; billAttached = toSqlKey 16
returnToBack             :: ActionResultId ; returnToBack = toSqlKey 17
returnToBillman          :: ActionResultId ; returnToBillman = toSqlKey 18
confirmedFinal           :: ActionResultId ; confirmedFinal = toSqlKey 19
confirmedWODirector      :: ActionResultId ; confirmedWODirector = toSqlKey 20
confirmedHead            :: ActionResultId ; confirmedHead = toSqlKey 21
directorToHead           :: ActionResultId ; directorToHead = toSqlKey 22
confirmedDirector        :: ActionResultId ; confirmedDirector = toSqlKey 23
accountToDirector        :: ActionResultId ; accountToDirector = toSqlKey 24
confirmedAccount         :: ActionResultId ; confirmedAccount = toSqlKey 25
confirmedAnalyst         :: ActionResultId ; confirmedAnalyst = toSqlKey 26
complaintManaged         :: ActionResultId ; complaintManaged = toSqlKey 27
falseCallBilled          :: ActionResultId ; falseCallBilled = toSqlKey 28
falseCallUnbilled        :: ActionResultId ; falseCallUnbilled = toSqlKey 29
communicated             :: ActionResultId ; communicated = toSqlKey 30
okButNoService           :: ActionResultId ; okButNoService = toSqlKey 31
supervisorClosed         :: ActionResultId ; supervisorClosed = toSqlKey 32
partnerDelayConfirmed    :: ActionResultId ; partnerDelayConfirmed = toSqlKey 33
partnerDelayNotConfirmed :: ActionResultId ; partnerDelayNotConfirmed =
                                               toSqlKey 34
transferred              :: ActionResultId ; transferred = toSqlKey 35
rushDefer                :: ActionResultId ; rushDefer = toSqlKey 36
callEnded                :: ActionResultId ; callEnded = toSqlKey 100
needAnotherService       :: ActionResultId ; needAnotherService = toSqlKey 101
