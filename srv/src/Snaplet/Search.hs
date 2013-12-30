{-# LANGUAGE QuasiQuotes, TemplateHaskell, ScopedTypeVariables #-}

module Snaplet.Search (Search, searchInit)  where

import Prelude hiding (pred)
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Lens hiding (from)

import qualified Data.Map as M
import Data.Monoid
import Data.Maybe
import Data.Either
import Data.String (fromString)
import Data.Pool
import Data.Text (Text, toLower)
import qualified Data.Text             as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as LB
import qualified Data.HashMap.Strict   as HM
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
import Carma.Model.Service
import Carma.Model.Service.Towage
import Carma.Model.Search

data Search b = Search
  {postgres :: Pool Connection
  ,_auth    :: Snaplet (AuthManager b)
  }
makeLenses ''Search

type SearchHandler b t = Handler b (Search b) t

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
            and p.role = r.role :: int
            and p.r
      |]


caseSearch :: SearchHandler b (Either String Aeson.Value)
caseSearch = do
  Just usr <- with auth currentUser
  let Just uid = userId usr
  lim      <- getLimit
  offset   <- getOffset
  args     <- getJsonBody
  withPG $ \c -> do
    cse_fields <- modelFields uid "case" c
    svc_fields <- modelFields uid "service" c
    tow_fields <- modelFields uid "towage" c
    casePreds  <- predicatesFromParams c args caseSearchParams
    srvPreds   <- predicatesFromParams c args serviceSearchParams
    towPreds   <- predicatesFromParams c args towageSearchParams
    case partitionEithers [casePreds, srvPreds, towPreds] of
      ([], preds) -> do
        s <- query_ c (mkQuery (concatPredStrings preds) lim offset)
        let s' = map (filterResults cse_fields svc_fields tow_fields . parse) s
        return $ Right $ reply lim offset $ merge s'
      (errs, _) -> return $ Left $ foldl (++) "" errs
  where
    -- FIXME: do something so Postgresql.Simple can parse json to Aeson.Value
    parse (a, b, c) =
      (fromMaybe (Aeson.object []) $ Aeson.decode a
      ,Aeson.decode (fromMaybe "" b)
      ,Aeson.decode (fromMaybe "" c)
      )
    filterResults cse svc _   (c, Just s, Nothing) =
      (fltModel cse c, fltModel svc s, Aeson.Object HM.empty)
    filterResults cse _    _  (c, Nothing, Nothing) =
      (fltModel cse c, Aeson.Object HM.empty, Aeson.Object HM.empty)
    filterResults cse svc tow (c, Just s, Just t) =
      (fltModel cse c, fltModel svc s, fltModel tow t)
    fltModel :: [Text] -> Aeson.Value -> Aeson.Value
    fltModel fs (Aeson.Object o) = Aeson.Object $
      foldl (\a k -> maybe a (\v -> HM.insert k v a) $ HM.lookup k o)
        HM.empty $ map toLower fs -- use lowercase fieldsnames because
                                  -- so will postgres
    merge [] = []
    merge (x:xs) = (merge' x) : merge xs
    merge' (c, s, t) = Aeson.object [ ("case",    c)
                                    , ("service", Aeson.toJSON s)
                                    , ("towage",  Aeson.toJSON t)
                                    ]

search :: SearchHandler b (Either String Aeson.Value) -> SearchHandler b ()
search = (>>= either (finishWithError 500) writeJSON)


mkQuery :: Text -> Int -> Int -> Query
mkQuery pred lim offset
  = fromString $ printf
      (  "    select row_to_json(casetbl.*)    :: text,"
      ++ "           row_to_json(servicetbl.*) :: text,"
      ++ "           row_to_json(towagetbl.*)  :: text"
      ++ "     from casetbl left join servicetbl"
      ++ "       on split_part(servicetbl.parentId, ':', 2)::int = casetbl.id"
      ++ "     left join towagetbl"
      ++ "       on servicetbl.id = towagetbl.id"
      ++ "     where (%s) limit %i offset %i;"
      )
      (T.unpack pred) lim offset


searchInit
  :: Pool Connection -> Snaplet (AuthManager b) -> SnapletInit b (Search b)
searchInit conn sessionMgr = makeSnaplet "search" "Search snaplet" Nothing $ do
  addRoutes [("services", method POST $ search caseSearch)]
  return $ Search conn sessionMgr

reply :: Int -> Int -> [Aeson.Value] -> Aeson.Value
reply lim offset val =
  let next = if length val < lim then Nothing else Just (offset + lim)
      prev = if offset <= 0      then Nothing else Just (offset - lim)
  in Aeson.object [ ("values", Aeson.toJSON val)
                  , ("next", Aeson.toJSON next)
                  , ("prev", Aeson.toJSON prev)
                  ]


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

getOffset :: SearchHandler b Int
getOffset
  = fromMaybe 0 . (>>= fmap fst . B.readInt)
  <$> getParam "offset"

writeJSON :: Aeson.ToJSON v => v -> Handler a b ()
writeJSON v = do
  modifyResponse $ setContentType "application/json"
  writeLBS $ Aeson.encode v
