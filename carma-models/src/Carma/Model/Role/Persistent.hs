{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.Role.Persistent where

import           Data.Typeable
import           Data.Text (Text)

import           Database.Persist.TH
import           Database.Persist.Sql (toSqlKey)


-- | @Role@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Role sql=Role
  label Text sql=label
  value Text sql=value
  isBack Bool sql=isback
  hidden Bool sql=hidden

  deriving Typeable Show
|]


-- | @Role@ predefined IDs.
core              :: RoleId ; core              = toSqlKey  1
call              :: RoleId ; call              = toSqlKey  2
parguy            :: RoleId ; parguy            = toSqlKey  3
userAdmin         :: RoleId ; userAdmin         = toSqlKey  4
userViewer        :: RoleId ; userViewer        = toSqlKey  5
lovAdmin          :: RoleId ; lovAdmin          = toSqlKey  6
lovViewer         :: RoleId ; lovViewer         = toSqlKey  7
reportManager     :: RoleId ; reportManager     = toSqlKey  8
billManager       :: RoleId ; billManager       = toSqlKey  9
billChecker       :: RoleId ; billChecker       = toSqlKey 10
vinAdmin          :: RoleId ; vinAdmin          = toSqlKey 11
supervisor        :: RoleId ; supervisor        = toSqlKey 12
head              :: RoleId ; head              = toSqlKey 13
back              :: RoleId ; back              = toSqlKey 14
psaanalyst        :: RoleId ; psaanalyst        = toSqlKey 15
searchCase        :: RoleId ; searchCase        = toSqlKey 16
searchCall        :: RoleId ; searchCall        = toSqlKey 17
searchContract    :: RoleId ; searchContract    = toSqlKey 18
partner           :: RoleId ; partner           = toSqlKey 19
contract_admin    :: RoleId ; contract_admin    = toSqlKey 20
bo_qa             :: RoleId ; bo_qa             = toSqlKey 22
bo_order          :: RoleId ; bo_order          = toSqlKey 23
bo_control        :: RoleId ; bo_control        = toSqlKey 24
bo_account        :: RoleId ; bo_account        = toSqlKey 25
bo_director       :: RoleId ; bo_director       = toSqlKey 26
bo_analyst        :: RoleId ; bo_analyst        = toSqlKey 27
bo_bill           :: RoleId ; bo_bill           = toSqlKey 28
bo_parguy         :: RoleId ; bo_parguy         = toSqlKey 29
bo_close          :: RoleId ; bo_close          = toSqlKey 30
bo_dealer         :: RoleId ; bo_dealer         = toSqlKey 31
vwfake            :: RoleId ; vwfake            = toSqlKey 32
dpViewer          :: RoleId ; dpViewer          = toSqlKey 34
programManager    :: RoleId ; programManager    = toSqlKey 35
sms               :: RoleId ; sms               = toSqlKey 40
bo_secondary      :: RoleId ; bo_secondary      = toSqlKey 41
hacker            :: RoleId ; hacker            = toSqlKey 42
bo_info           :: RoleId ; bo_info           = toSqlKey 43
badmin            :: RoleId ; badmin            = toSqlKey 44
cti               :: RoleId ; cti               = toSqlKey 50
consultant_op     :: RoleId ; consultant_op     = toSqlKey 60
consultant_mech   :: RoleId ; consultant_mech   = toSqlKey 61
consultant_tech   :: RoleId ; consultant_tech   = toSqlKey 62
bo_urgent         :: RoleId ; bo_urgent         = toSqlKey 63
bo_orderRefs      :: RoleId ; bo_orderRefs      = toSqlKey 64
bo_orderAvarcom   :: RoleId ; bo_orderAvarcom   = toSqlKey 65
bo_controlRefs    :: RoleId ; bo_controlRefs    = toSqlKey 66
bo_controlAvarcom :: RoleId ; bo_controlAvarcom = toSqlKey 67
