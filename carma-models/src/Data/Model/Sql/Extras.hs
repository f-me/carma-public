{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Model.Sql.Extras () where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow


-- WARNING! It is orphan instance!
instance {-# OVERLAPS #-} FromRow a => FromRow (a :. ()) where
  fromRow = fmap (:. ()) fromRow
