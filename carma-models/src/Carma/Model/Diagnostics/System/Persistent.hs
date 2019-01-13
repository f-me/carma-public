{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.Diagnostics.System.Persistent where

import           Data.Typeable
import           Data.Text (Text)

import           Database.Persist.TH


-- | @System@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
System sql=System
  label Text sql=label
  deriving Typeable Show
|]
