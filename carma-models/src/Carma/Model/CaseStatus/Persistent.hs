{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.CaseStatus.Persistent where

import           Data.Typeable
import           Data.Text (Text)

import           Database.Persist.Sql (toSqlKey)
import           Database.Persist.TH


-- | @CaseStatus@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
CaseStatus sql=CaseStatus
  label Text sql=label
  deriving Typeable Show
|]


-- | @CaseStatus@ predefined IDs.
front, needInfo, back, closed, canceled, mobileOrder, mobileAccident
  :: CaseStatusId
front = toSqlKey 1
needInfo = toSqlKey 2
back = toSqlKey 3
closed = toSqlKey 4
canceled = toSqlKey 5
mobileOrder = toSqlKey 6
mobileAccident = toSqlKey 7
