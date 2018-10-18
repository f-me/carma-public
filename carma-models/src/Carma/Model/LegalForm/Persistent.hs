{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.LegalForm.Persistent where

import           Data.Text (Text)
import           Data.Typeable

import           Database.Persist.TH
import           Database.Persist.Sql (toSqlKey)


-- | @LegalForm@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
LegalForm sql=LegalForm
  value    Text         sql=value
  label    Text         sql=label
  synonyms [Text] Maybe sql=synonyms

  deriving Typeable Show
|]


-- | @LegalForm@ predefined IDs.
person, company :: LegalFormId
person = toSqlKey 1
company = toSqlKey 2
