
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model.Patch.Sql
  where

import Prelude hiding (read)

import Data.Int (Int64)
import Data.String
import Text.Printf

import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Database.PostgreSQL.Simple

import Data.Model
import Data.Model.Patch


read :: forall m . Model m => Ident m -> Connection -> IO [Patch m]
read (Ident i) c = query c (fromString q) [i]
  where
    fields = [fd_name f | f <- modelFields :: [FieldDesc m]]
    q = printf "SELECT %s FROM %s WHERE id = ?"
      (T.unpack $ T.intercalate ", " fields)
      (tableName (undefined :: m))


readMany :: forall m . Model m => Connection -> IO [Patch m]
readMany c = query_ c (fromString q)
  where
    fields = [fd_name f | f <- modelFields :: [FieldDesc m]]
    q = printf "SELECT %s FROM %s"
      (T.unpack $ T.intercalate ", " fields)
      (tableName (undefined :: m))
