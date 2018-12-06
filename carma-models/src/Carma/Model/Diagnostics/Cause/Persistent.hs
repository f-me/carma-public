{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.Diagnostics.Cause.Persistent where

import           Data.Typeable
import           Data.Text (Text)

import           Database.Persist.TH


-- | @Cause@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Cause sql=Cause
  label Text sql=label
  fdds Text Maybe sql=fdds
  deriving Typeable Show
|]
