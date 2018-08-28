{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.CaseSource.Persistent where

import           Data.Typeable
import           Data.Text (Text)

import           Database.Persist.Sql (toSqlKey)
import           Database.Persist.TH


-- | @CaseSource@ persistent model.
mkPersist sqlSettings [persistLowerCase|
CaseSource sql=CaseSource
  label Text sql=label
  deriving Typeable Show
|]


-- | @CaseSource@ predefined IDs.
--
-- TODO add Era Glonass source.
op, mobile, mobileAccident :: CaseSourceId
op = toSqlKey 1
mobile = toSqlKey 2
mobileAccident = toSqlKey 3
