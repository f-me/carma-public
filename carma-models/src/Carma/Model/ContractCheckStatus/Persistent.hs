{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.ContractCheckStatus.Persistent where

import           Data.Typeable
import           Data.Text (Text)

import           Database.Persist.Sql (toSqlKey)
import           Database.Persist.TH


-- | @ContractCheckStatus@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ContractCheckStatus sql=ContractCheckStatus
  label Text sql=label
  deriving Typeable Show
|]


-- | @ContractCheckStatus@ predefined IDs.
base, vinExpired :: ContractCheckStatusId
base = toSqlKey 1
vinExpired = toSqlKey 7
