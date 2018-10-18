{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

-- | @ContractRegistrationReason@ persistent model.
module Carma.Model.ContractRegistrationReason.Persistent where

import           Data.Text (Text)
import           Data.Typeable

import           Database.Persist.TH

import           Database.Persist.Postgresql.JSON ()
                 -- ^ @PersistFieldSql Value@ instance


-- | @ContractRegistrationReason@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ContractRegistrationReason json sql=ContractRegistrationReason
  label Text sql=label

  deriving Typeable Show
|]
