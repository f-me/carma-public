{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.ActionType.Persistent where

import           Data.Typeable
import           Data.Text (Text)

import           Database.Persist.TH
import           Database.Persist.Sql (toSqlKey)


-- | @ActionType@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ActionType sql=ActionType
  label      Text sql=label
  desc       Text sql=description
  priority   Int  sql=priority
  maxSeconds Int  sql=maxseconds

  deriving Typeable Show
|]


-- | @ActionType@ predefined IDs.

orderService        :: ActionTypeId ; orderService        = toSqlKey 1
orderServiceAnalyst :: ActionTypeId ; orderServiceAnalyst = toSqlKey 2
tellClient          :: ActionTypeId ; tellClient          = toSqlKey 3
checkStatus         :: ActionTypeId ; checkStatus         = toSqlKey 4
needPartner         :: ActionTypeId ; needPartner         = toSqlKey 5
checkEndOfService   :: ActionTypeId ; checkEndOfService   = toSqlKey 6
closeCase           :: ActionTypeId ; closeCase           = toSqlKey 7
getDealerInfo       :: ActionTypeId ; getDealerInfo       = toSqlKey 8
cancelService       :: ActionTypeId ; cancelService       = toSqlKey 9
makerApproval       :: ActionTypeId ; makerApproval       = toSqlKey 10
tellMakerDeclined   :: ActionTypeId ; tellMakerDeclined   = toSqlKey 11
addBill             :: ActionTypeId ; addBill             = toSqlKey 12
billmanNeedInfo     :: ActionTypeId ; billmanNeedInfo     = toSqlKey 13
headCheck           :: ActionTypeId ; headCheck           = toSqlKey 14
directorCheck       :: ActionTypeId ; directorCheck       = toSqlKey 15
accountCheck        :: ActionTypeId ; accountCheck        = toSqlKey 16
analystCheck        :: ActionTypeId ; analystCheck        = toSqlKey 17
complaintResolution :: ActionTypeId ; complaintResolution = toSqlKey 18
tellMeMore          :: ActionTypeId ; tellMeMore          = toSqlKey 19
callMeMaybe         :: ActionTypeId ; callMeMaybe         = toSqlKey 20
checkDispatchTime   :: ActionTypeId ; checkDispatchTime   = toSqlKey 21
accident            :: ActionTypeId ; accident            = toSqlKey 22
confirmPartnerDelay :: ActionTypeId ; confirmPartnerDelay = toSqlKey 23
rushOrder           :: ActionTypeId ; rushOrder           = toSqlKey 24
call                :: ActionTypeId ; call                = toSqlKey 100
