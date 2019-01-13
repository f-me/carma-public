{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.Diagnostics.Suggestion.Persistent where

import           Data.Typeable
import           Data.Text (Text)

import           Database.Persist.TH


-- | @Suggestion@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Suggestion sql=Suggestion
  label Text sql=label
  deriving Typeable Show
|]
