{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.City.Persistent where

import           Data.Typeable
import           Data.Text (Text)

import           Database.Persist.TH


-- | Partially implemented @City@ persistent model.
--
-- __TODO__ Implement @Coords@ type.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
City sql=City
  label Text sql=label
  value Text sql=value
  -- TODO coords Coords Maybe sql=coords
  timezone Text sql=timezone
  deriving Typeable Show
|]
