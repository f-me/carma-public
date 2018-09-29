{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.Transmission.Persistent where

import           Data.Typeable
import           Data.Text (Text)

import           Database.Persist.TH


-- | @Transmission@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Transmission sql=Transmission
  label Text sql=label
  synonyms [Text] Maybe sql=synonyms

  deriving Typeable Show
|]
