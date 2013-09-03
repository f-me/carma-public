
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model.Patch.Sql
  (create
  ,read
  ,readMany
  ,update
  ) where

import Prelude hiding (read)

import Control.Applicative
import Data.Int (Int64)
import Data.String
import Text.Printf

import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Database.PostgreSQL.Simple

import Data.Model
import Data.Model.Patch


create :: forall m . Model m => Patch m -> Connection -> IO (Ident m)
create p c = head . head <$> query c (fromString q) p
  where
    m = untypedPatch p
    -- we use `map fst . HashMap.toList` instead of `HashMap.keys`
    -- just to be sure that `insFields` are in the same order as
    -- `ToRow (Patch m)` expects
    insFields = map fst $ HashMap.toList m
    q = printf "INSERT INTO %s (%s) VALUES (%s) RETURNING id"
      (show $ tableName (undefined :: m))
      (T.unpack $ T.intercalate ", " insFields)
      (T.unpack $ T.intercalate ", " $ replicate (length insFields) "?")


read :: forall m . Model m => Ident m -> Connection -> IO [Patch m]
read (Ident i) c = query c (fromString q) [i]
  where
    fields = [fd_name f | f <- identDesc : modelFields :: [FieldDesc m]]
    q = printf "SELECT %s FROM %s WHERE id = ?"
      (T.unpack $ T.intercalate ", " fields)
      (show $ tableName (undefined :: m))


readMany :: forall m . Model m => Int64 -> Int64 -> Connection -> IO [Patch m]
readMany lim off c = query_ c (fromString q)
  where
    fields = [fd_name f | f <- identDesc : modelFields :: [FieldDesc m]]
    q = printf "SELECT %s FROM %s ORDER BY id LIMIT %i OFFSET %i"
      (T.unpack $ T.intercalate ", " fields)
      (show $ tableName (undefined :: m))
      lim off


update :: forall m . Model m => Ident m -> Patch m -> Connection -> IO Int64
update (Ident i) p c = execute c (fromString q) p
  where
    m = untypedPatch p
    -- we use `map fst . HashMap.toList` instead of `HashMap.keys`
    -- just to be sure that `insFields` are in the same order as
    -- `ToRow (Patch m)` expects
    updFields = map (T.concat . (:["=?"]) . fst) $ HashMap.toList m
    q = printf "UPDATE %s SET %s WHERE id = %d"
      (T.unpack $ T.intercalate ", " updFields)
      (show $ tableName (undefined :: m))
      i
