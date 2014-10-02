{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}

module Data.Model.Sql
  ( select
  , selectJSON
  , selectPatch
  , update
  , eq
  , fullPatch
  , isNull
  , sql_in
  , descBy
  , ascBy

  , SqlQ(..)
  , SqlP
  ) where

import Text.Printf (printf)
import Data.String (fromString)
import Data.List (intercalate)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField hiding (Field)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Data.Aeson (ToJSON(..), fromJSON, Result)
import qualified Data.Aeson as Aeson

import Data.Singletons

import Data.Model
import Data.Model.Patch

select :: (FromRow (QRes q), SqlQ q) => q -> Connection -> IO [QRes q]
select q c = uncurry (query c) $ mkSelect q


update :: (SqlQ how, SqlQ which, QMod how ~ QMod which) =>
          how
       -- ^ How to transform target set of rows.
       -> which
       -- ^ Which rows to select for UPDATE.
       -> Connection -> IO Int64
update how which c = uncurry (execute c) $ mkUpdate how which


mkUpdate :: (SqlQ how, SqlQ which, QMod how ~ QMod which) =>
            how -> which -> (Query, QArg how :. QArg which)
mkUpdate how which =
    let
        predChunks :: String -> [String] -> String
        predChunks glue ps = intercalate glue ps
    in
      case queryPredicate how of
        [] -> error "No new field values provided"
        news ->
            (fromString
             $ printf "UPDATE %s SET %s"
             (show $ queryTbl how)
             (predChunks ", " news)
             ++ case queryPredicate which of
                  []    -> ""
                  conds -> " WHERE " ++ predChunks " AND " conds
            , queryArgs how :. queryArgs which
            )



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

selectPatch
  :: (ToValueList (QRes q), SqlQ q, FromRow (QRes q), Model m)
  => q -> Connection -> IO (Result [Patch m])
selectPatch q c = selectJSON q c >>= return . mapM fromJSON

mkSelect :: SqlQ q => q -> (Query, QArg q)
mkSelect q =
  (fromString
    $ printf "SELECT %s FROM %s"
      (T.unpack $ T.intercalate ", " $ queryProjection q)
      (show $ queryTbl q)
    ++ case queryPredicate q of
      [] -> ""
      ps -> " WHERE " ++ intercalate " AND " ps ++
            (case queryOrdering q of
               [] -> ""
               chunks -> " ORDER BY " ++ (T.unpack $ T.intercalate ", " chunks))
  ,queryArgs q
  )

class (ToRow (QArg q), Model (QMod q)) => SqlQ q where
  type QRes q
  type QArg q
  type QMod q
  queryProjection :: q -> [Text]
  queryPredicate  :: q -> [String]
  queryOrdering   :: q -> [Text]
  queryTbl       :: q -> Text
  queryArgs       :: q -> QArg q


data FullProj m = FullProj


-- | Obtain 'FullPatch' using 'select'.
fullPatch :: (m -> PK t m d) -> FullProj m
fullPatch _ = FullProj


instance (Model m) => SqlQ (FullProj m) where
  type QRes (FullProj m) = FullPatch m
  type QArg (FullProj m) = ()
  type QMod (FullProj m) = m
  queryProjection _ = map fd_name $ onlyDefaultFields $
                      modelFields (modelInfo :: ModelInfo m)
  queryPredicate  _ = []
  queryOrdering   _ = []
  queryTbl        _ = tableName (modelInfo :: ModelInfo m)
  queryArgs       _ = ()


instance (Model m, SqlQ q, QMod q ~ m) => SqlQ ((FullProj m) :. q) where
  type QRes ((FullProj m) :. q) = FullPatch m :. QRes q
  type QArg ((FullProj m) :. q) = QArg q
  type QMod ((FullProj m) :. q) = m
  queryProjection (f :. q) = queryProjection f ++ queryProjection q
  queryPredicate  (_ :. q) = queryPredicate q
  queryOrdering   (_ :. q) = queryOrdering q
  queryTbl         _       = tableName (modelInfo :: ModelInfo m)
  queryArgs       (_ :. q) = queryArgs q


instance (Model m, SingI nm, FromField t)
    => SqlQ (m -> Field t (FOpt nm desc app))
  where
    type QRes (m -> Field t (FOpt nm desc app)) = Only t
    type QArg (m -> Field t (FOpt nm desc app)) = ()
    type QMod (m -> Field t (FOpt nm desc app)) = m
    queryProjection f = [fieldName f]
    queryPredicate  _ = []
    queryOrdering   _ = []
    queryTbl       _  = tableName (modelInfo :: ModelInfo m)
    queryArgs       _ = ()

instance (Model m, SingI nm, FromField t, SqlQ q, QMod q ~ m)
    => SqlQ ((m -> Field t (FOpt nm desc app)) :. q)
  where
    type QRes ((m -> Field t (FOpt nm desc app)) :. q) = Only t :. QRes q
    type QArg ((m -> Field t (FOpt nm desc app)) :. q) = QArg q
    type QMod ((m -> Field t (FOpt nm desc app)) :. q) = m
    queryProjection (f :. q) = fieldName f : queryProjection q
    queryPredicate  (_ :. q) = queryPredicate q
    queryOrdering   (_ :. q) = queryOrdering q
    queryTbl       _        = tableName (modelInfo :: ModelInfo m)
    queryArgs       (_ :. q) = queryArgs q

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
  queryOrdering   _ = []
  queryTbl       _ = tableName (modelInfo :: ModelInfo m)
  queryArgs       p = Only $ sqlP_argValue p

instance (Model m, ToField t, SqlQ q, QMod q ~ m)
    => SqlQ (SqlP m t :. q)
  where
    type QRes (SqlP m t :. q) = QRes q
    type QArg (SqlP m t :. q) = Only t :. QArg q
    type QMod (SqlP m t :. q) = m
    queryProjection  (_ :. q) = queryProjection q
    queryPredicate   (p :. q)
      = printf "%s %s ?" (sqlP_fieldName p) (sqlP_op p)
      : queryPredicate q
    queryOrdering    (_ :. q) = queryOrdering q
    queryTbl        _        = tableName (modelInfo :: ModelInfo m)
    queryArgs        (p :. q) = Only (sqlP_argValue p) :. queryArgs q


eq
  :: (Model m, SingI nm)
  => (m -> Field t (FOpt nm desc app)) -> t
  -> SqlP m t
eq f v = SqlP (T.unpack $ fieldName f) v "="


isNull
  :: (Model m, SingI nm)
  => (m -> Field (Maybe t) (FOpt nm desc app))
  -> SqlP m (Maybe t)
isNull f = SqlP (T.unpack $ fieldName f) Nothing "IS"


sql_in :: (Model m, SingI nm)
       => (m -> Field t (FOpt nm desc app)) -> [t]
       -> SqlP m (In [t])
sql_in f v = SqlP (T.unpack $ fieldName f) (In v) "IN"


-- | SQL @ORDER BY@ predicate.
data SqlO m = SqlO
  {sqlO_fieldName :: Text
  ,sqlO_order     :: Text
  }


descBy
  :: (Model m, SingI nm)
  => (m -> Field t (FOpt nm desc app))
  -> SqlO m
descBy f = SqlO (fieldName f) "DESC"


ascBy
  :: (Model m, SingI nm)
  => (m -> Field t (FOpt nm desc app))
  -> SqlO m
ascBy f = SqlO (fieldName f) "ASC"


instance (Model m) => SqlQ (SqlO m) where
  type QRes (SqlO m) = ()
  type QArg (SqlO m) = ()
  type QMod (SqlO m) = m
  queryProjection _ = []
  queryPredicate  _ = []
  queryOrdering   p = [T.concat [sqlO_fieldName p, " ", sqlO_order p]]
  queryTbl        _ = tableName (modelInfo :: ModelInfo m)
  queryArgs       _ = ()


instance (Model m) => SqlQ (SqlO m :. SqlO m) where
  type QRes (SqlO m :. SqlO m) = ()
  type QArg (SqlO m :. SqlO m) = ()
  type QMod (SqlO m :. SqlO m) = m
  queryProjection _ = []
  queryPredicate  _ = []
  queryOrdering (p :. q) =
    queryOrdering p ++ queryOrdering q
  queryTbl        _ = tableName (modelInfo :: ModelInfo m)
  queryArgs       _ = ()
