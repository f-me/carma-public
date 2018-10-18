{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

-- | @CheckType@ persistent model.
module Carma.Model.CheckType.Persistent where

import           Data.Text (Text)
import           Data.Typeable

import           Database.Persist.TH

import           Database.Persist.Postgresql.JSON ()
                 -- ^ @PersistFieldSql Value@ instance


-- | @CheckType@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
CheckType json sql=CheckType
  label Text sql=label
  synonyms [Text] Maybe sql=synonyms

  deriving Typeable Show
|]
