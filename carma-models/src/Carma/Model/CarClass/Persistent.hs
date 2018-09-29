{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.CarClass.Persistent where

import           Data.Typeable
import           Data.Text (Text)

import           Database.Persist.Sql (toSqlKey)
import           Database.Persist.TH


-- | @CarClass@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
CarClass sql=CarClass
  label Text sql=label
  synonyms [Text] Maybe sql=synonyms

  deriving Typeable Show
|]


-- | @CarClass@ predefined IDs.
psab, psam1, psam2, psah :: CarClassId
psab = toSqlKey 7
psam1 = toSqlKey 8
psam2 = toSqlKey 9
psah = toSqlKey 10
