{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.ProgramType.Persistent where

import           Data.Typeable
import           Data.Text (Text)

import           Database.Persist.Sql (toSqlKey)
import           Database.Persist.TH


-- | @ProgramType@ persistent model.
mkPersist sqlSettings [persistLowerCase|
ProgramType sql=ProgramType
  label Text sql=label
  deriving Typeable Show
|]


-- | @ProgramType@ predefined IDs.
b2b, b2c :: ProgramTypeId
b2b = toSqlKey 1
b2c = toSqlKey 2
