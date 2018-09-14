{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.Usermeta.Persistent where

import           Data.Typeable

import           Database.Persist.Sql (toSqlKey)
import           Database.Persist.TH


-- | Partially implemented @Usermeta@ persistent model.
--
-- For now just for IDs.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Usermeta sql=usermetatbl
  deriving Typeable Show
|]


-- | @Usermeta@ predefined IDs.
psa, arc, admin :: UsermetaId
psa = toSqlKey 387
arc = toSqlKey 728
admin = toSqlKey 90
