
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}

module Data.Model.Sql
  (select
  ,selectJSON
  ,eq
  ) where

import Text.Printf (printf)
import Data.String (fromString)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField hiding (Field)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Data.Aeson (ToJSON(..))
import qualified Data.Aeson as Aeson
import GHC.TypeLits

import Data.Model


select :: (FromRow (QRes q), SqlQ q) => q -> Connection -> IO [QRes q]
select q c = uncurry (query c) $ mkSelect q


class ToValueList xs where
  toValueList :: xs -> [Aeson.Value]
instance ToJSON x => ToValueList (Only x) where
  toValueList (Only x) = [toJSON x]
instance (ToJSON x, ToValueList xs) => ToValueList (Only x :. xs) where
  toValueList (Only x :. xs) = toJSON x : toValueList xs
instance ToJSON x => ToValueList (Only x :. ()) where
  toValueList (Only x :. ()) = [toJSON x]

selectJSON
  :: (ToValueList (QRes q), SqlQ q, FromRow (QRes q))
  => q -> Connection -> IO [Aeson.Value]
selectJSON q c = select q c >>= return
  . map
    ( Aeson.object
    . zip (queryProjection q)
    . toValueList
    )

mkSelect :: SqlQ q => q -> (Query, QArg q)
mkSelect q =
  (fromString
    $ printf "SELECT %s FROM %s"
      (T.unpack $ T.intercalate ", " $ queryProjection q)
      (show $ queryFrom q)
    ++ case queryPredicate q of
      [] -> ""
      ps -> " WHERE " ++ concat (intersperse " AND " ps)
  ,queryArgs q
  )


class (ToRow (QArg q), Model (QMod q)) => SqlQ q where
  type QRes q
  type QArg q
  type QMod q
  queryProjection :: q -> [Text]
  queryPredicate  :: q -> [String]
  queryFrom       :: q -> Text
  queryArgs       :: q -> QArg q


instance (Model m, SingI nm, FromField t)
    => SqlQ (m -> Field t (FOpt nm desc))
  where
    type QRes (m -> Field t (FOpt nm desc)) = Only t
    type QArg (m -> Field t (FOpt nm desc)) = ()
    type QMod (m -> Field t (FOpt nm desc)) = m
    queryProjection f = [fieldName f]
    queryPredicate  f = []
    queryFrom       _ = tableName (modelInfo :: ModelInfo m)
    queryArgs       _ = ()

instance (Model m, SingI nm, FromField t, SqlQ q, QMod q ~ m)
    => SqlQ ((m -> Field t (FOpt nm desc)) :. q)
  where
    type QRes ((m -> Field t (FOpt nm desc)) :. q) = Only t :. QRes q
    type QArg ((m -> Field t (FOpt nm desc)) :. q) = QArg q
    type QMod ((m -> Field t (FOpt nm desc)) :. q) = m
    queryProjection (f :. q) = fieldName f : queryProjection q
    queryPredicate  (f :. q) = queryPredicate q
    queryFrom       _        = tableName (modelInfo :: ModelInfo m)
    queryArgs       (f :. q) = queryArgs q

-- NB!
instance FromRow a => FromRow (a :. ()) where
  fromRow = fmap (:. ()) fromRow


data SqlP m t = SqlP
  {sqlP_fieldName :: String
  ,sqlP_argValue  :: t
  ,sqlP_op        :: String
  }

instance (Model m, ToField t) => SqlQ (SqlP m t) where
  type QRes (SqlP m t) = ()
  type QArg (SqlP m t) = Only t
  type QMod (SqlP m t) = m
  queryProjection _ = []
  queryPredicate  p = [printf "%s %s ?" (sqlP_fieldName p) (sqlP_op p)]
  queryFrom       _ = tableName (modelInfo :: ModelInfo m)
  queryArgs       p = Only $ sqlP_argValue p

instance (Model m, ToField t, SqlQ q, QMod q ~ m)
    => SqlQ (SqlP m t :. q)
  where
    type QRes (SqlP m t :. q) = QRes q
    type QArg (SqlP m t :. q) = Only t :. QArg q
    type QMod (SqlP m t :. q) = m
    queryProjection  (p :. q) = queryProjection q
    queryPredicate   (p :. q)
      = printf "%s %s ?" (sqlP_fieldName p) (sqlP_op p)
      : queryPredicate q
    queryFrom        _        = tableName (modelInfo :: ModelInfo m)
    queryArgs        (p :. q) = Only (sqlP_argValue p) :. queryArgs q


eq
  :: (Model m, SingI nm)
  => (m -> Field t (FOpt nm desc)) -> t
  -> SqlP m t
eq f v = SqlP (T.unpack $ fieldName f) v "="
