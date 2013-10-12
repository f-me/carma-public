{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Snaplet.Search (Search, searchInit)  where

import Prelude hiding (pred)
import Control.Applicative
import Control.Monad.State
import Control.Lens hiding (from)

import Data.Maybe
import Data.String (fromString)
import Data.Pool
import Data.Text (Text)
import qualified Data.Text             as T
import qualified Data.ByteString.Char8 as B
import Text.Printf

import qualified Data.Aeson as Aeson

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.SqlQQ

import Util
import Utils.HttpErrors

import Carma.Model.Case


data Search b = Search
  {postgres :: Pool Connection
  ,_auth    :: Snaplet (AuthManager b)
  }
makeLenses ''Search

type SearchHandler b t = Handler b (Search b) t


services :: SearchHandler b ()
services = do
  l <- getLimit
  b <- getJsonBody
  q <- withPG $ \c -> buildCaseSearchQ c l b
  -- j <- withPG $ \c -> query_ c $ buildCaseSearchQ c l b
  case q of
    Left  v  -> writeBS $ B.pack v
    Right q' -> do
      v <- withPG $ \c -> query_ c $ fromString $ T.unpack q'
      writeBS $ B.concat $ map B.concat v


modelFields :: UserId -> Text -> PG.Connection -> IO [Text]
modelFields uid modelName c
  = concat <$> query c q (modelName, unUid uid)
  where
    q = [sql|
      with
        model_name as (select (? || 'tbl')::text as table),
        usr_role as
          (select unnest(roles) as role from usermetatbl where uid = ?),
        svc_field as
          (select column_name as column
            from information_schema.columns, model_name
            where table_name = model_name.table),
        svc_model as
          (select c1.relname as table
            from pg_inherits p, pg_class c1, pg_class c2, model_name
            where p.inhrelid = c1.oid
              and p.inhparent = c2.oid
              and c2.relname = model_name.table)
        select distinct field
          from "FieldPermission" p, svc_field f, usr_role r,
            (select * from svc_model union select * from model_name) m
          where (model || 'tbl') = m.table
            and field ilike f.column
            and p.role = r.role
            and r
      |]


data Projection
  = ProjField {proj_table, proj_field, proj_alias :: Text}
  | ProjFn    {proj_fn, proj_alias :: Text}



caseSearch :: SearchHandler b (Either String [Aeson.Value])
caseSearch = do
  Just usr <- with auth currentUser
  let Just uid = userId usr
  lim      <- getLimit
  args     <- getJsonBody
  withPG $ \c -> do
    cse_fields <- modelFields uid "case" c
    svc_fields <- modelFields uid "service" c
    let proj
          =  [ProjField "casetbl"    f (T.append "cse_" f) | f <- cse_fields]
          ++ [ProjField "servicetbl" f (T.append "svc_" f) | f <- svc_fields]
          ++ [ProjFn
              (T.concat
                ["(case when servicetbl.type = 'towage' then"
                ," (select towaddress_address"
                ,"  from towagetbl t where t.id = servicetbl.id limit 1)"
                ," else '' end)"
                ])
              "svc_towAddr"]
    caseSearchPredicate c args >>= \case
      Left err -> return $ Left err
      Right pred -> Right . join <$> query_ c
        (mkQuery proj pred lim
          (T.concat
            ["casetbl JOIN servicetbl ON"
            ," split_part(servicetbl.parentId, ':', 2)::int = casetbl.id"
            ]))

search :: SearchHandler b (Either String [Aeson.Value]) -> SearchHandler b ()
search = (>>= either (finishWithError 500) writeJSON)


mkQuery
   :: [Projection] -> Text -> Int -> Text
   -> Query
mkQuery proj pred lim from
  = fromString
  $ printf "SELECT row_to_json(r) FROM (SELECT %s FROM %s WHERE %s LIMIT %i) r"
    (T.unpack $ T.intercalate ", " $ map mkProj proj)
    (T.unpack from)
    (T.unpack pred)
    lim
  where
    mkProj = \case
      ProjField t f a -> T.concat [t, ".", f, " as ", a]
      ProjFn f a      -> T.concat ["(", f, ") as ", a]


searchInit
  :: Pool Connection -> Snaplet (AuthManager b) -> SnapletInit b (Search b)
searchInit conn sessionMgr = makeSnaplet "search" "Search snaplet" Nothing $ do
  addRoutes [("services", method POST $ search caseSearch)]
  return $ Search conn sessionMgr



-- Utils
----------------------------------------------------------------------
withPG :: (Connection -> IO a) -> SearchHandler b a
withPG f = gets postgres >>= liftIO . (`withResource` f)

getJsonBody :: SearchHandler b Aeson.Value
getJsonBody = Util.readJSONfromLBS <$> readRequestBody 4096

getLimit :: SearchHandler b Int
getLimit
  = fromMaybe 10 . (>>= fmap fst . B.readInt)
  <$> getParam "limit"

writeJSON :: Aeson.ToJSON v => v -> Handler a b ()
writeJSON v = do
  modifyResponse $ setContentType "application/json"
  writeLBS $ Aeson.encode v
