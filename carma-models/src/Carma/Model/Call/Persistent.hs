{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.Call.Persistent where

import           Data.Typeable

import           Database.Persist.TH


-- | Partially implemented @Call@ persistent model.
--
-- For now just for IDs.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Call sql=calltbl
  deriving Typeable Show
|]
