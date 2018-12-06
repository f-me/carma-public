{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.Diagnostics.Wazzup.Persistent where

import           Data.Typeable
import           Data.Text (Text)

import           Database.Persist.TH

import           Carma.Model.Diagnostics.System.Persistent (SystemId)
import           Carma.Model.Diagnostics.Part.Persistent (PartId)
import           Carma.Model.Diagnostics.Cause.Persistent (CauseId)
import           Carma.Model.Diagnostics.Suggestion.Persistent (SuggestionId)


-- | @Wazzup@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Wazzup sql=Wazzup
  label Text sql=label
  system SystemId Maybe sql=system
  part PartId Maybe sql=part
  cause CauseId Maybe sql=cause
  suggestion SuggestionId Maybe sql=suggestion
  deriving Typeable Show
|]
