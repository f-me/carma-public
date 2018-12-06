{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

-- | @TaxScheme@ persistent model.
module Carma.Model.TaxScheme.Persistent where

import           Data.Text (Text)
import           Data.Typeable

import           Database.Persist.TH

import           Database.Persist.Postgresql.JSON ()
                 -- ^ @PersistFieldSql Value@ instance


-- | @TaxScheme@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
TaxScheme json sql=TaxScheme
  label Text sql=label

  deriving Typeable Show
|]
