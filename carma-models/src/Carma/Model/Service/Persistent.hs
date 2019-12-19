{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.Service.Persistent where

import           Data.Typeable

import           Database.Persist.TH


-- | Partially implemented @Service@ persistent model.
--
-- For now just for IDs.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Service sql=servicetbl
  deriving Typeable Show
|]
