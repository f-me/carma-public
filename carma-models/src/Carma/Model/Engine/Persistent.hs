{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.Engine.Persistent where

import           Data.Text
import           Data.Typeable

import           Database.Persist.Sql (toSqlKey)
import           Database.Persist.TH


-- | @Engine@ persistent model.
mkPersist sqlSettings [persistLowerCase|
Engine sql=Engine
  label Text sql=label
  synonyms Text Vector Maybe sql=synonyms

  deriving Typeable Show
|]


-- | @Engine@ predefined IDs.
petrol, diesel, hydrogen, electricity, lpg, lng :: EngineId
petrol = toSqlKey 1
diesel = toSqlKey 2
hydrogen = toSqlKey 3
electricity = toSqlKey 4
lpg = toSqlKey 5
lng = toSqlKey 6
