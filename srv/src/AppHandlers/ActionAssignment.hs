module AppHandlers.ActionAssignment where

import Prelude hiding (log)
import Control.Monad
import Control.Applicative
import Data.String (fromString)

import Data.List (intercalate)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import qualified Data.Aeson as Aeson

import Snap
import Snap.Snaplet.Auth
import Snap.Snaplet.SimpleLog
import qualified Snaplet.DbLayer as DB
import Database.PostgreSQL.Simple
----------------------------------------------------------------------
import Application
import AppHandlers.CustomSearches
import AppHandlers.Util

import Snaplet.Auth.PGUsers


assignQ :: Int -> AuthUser -> UserMeta -> [Text] -> Query
assignQ pri usr meta logdUsers = fromString
  $  "UPDATE actiontbl SET assignedTo = '" ++ uLogin ++ "'"
  ++ "  WHERE id = (SELECT act.id"
  ++ "    FROM (actiontbl act LEFT JOIN servicetbl svc"
  ++ "      ON  svc.type::text = substring(act.parentId, '(.*):')"
  ++ "      AND svc.id::text = substring(act.parentId, ':(.*)')),"
  ++ "      casetbl c"
  ++ "    WHERE closed = false"
  ++ "    AND c.id::text = substring(act.caseId, ':(.*)')"
  ++ "    AND priority = '" ++ show pri ++ "'"
  ++ "    AND duetime at time zone 'UTC' - now() < interval '30 minutes'"
  ++ "    AND targetGroup IN ('" ++ uRoles ++ "')"
  ++ "    AND (assignedTo IS NULL"
  ++ "         OR assignedTo NOT IN ('" ++ logdUsersList ++ "'))"
  ++ "    ORDER BY"
  ++ maybe "" (\set -> "(c.program IN ('" ++ set ++ "')) DESC,") programSet
  ++ maybe "" (\set -> "(c.city IN ('" ++ set ++ "')) DESC,") citySet
  ++ "      (act.name IN ('orderService', 'orderServiceAnalyst')"
  ++ "        AND coalesce(svc.urgentService, 'notUrgent') <> 'notUrgent') DESC,"
  ++ "      (CASE WHEN act.name IN ('orderService', 'orderServiceAnalyst')"
  ++ "        THEN coalesce(svc.times_expectedServiceStart,act.duetime)"
  ++ "        ELSE act.duetime"
  ++ "        END) ASC"
  ++ "    LIMIT 1)"
  ++ "  RETURNING id::text;"
  where
    uLogin = T.unpack $ userLogin usr
    uRoles = intercalate "','" [B.unpack r | Role r <- metaRoles meta]
    logdUsersList = T.unpack $ T.intercalate "','" logdUsers
    mkSet = B.unpack . B.intercalate "','"
    citySet = case boCities meta of
      Just c | not $ null c -> Just $ mkSet c
      _ -> Nothing
    programSet = case boPrograms meta of
      Just p | not $ null p -> Just $ mkSet p
      _ -> Nothing


littleMoreActionsHandler :: AppHandler ()
littleMoreActionsHandler = scoper "littleMoreActions" $ do
  Just cUsr <- with auth currentUser
  -- Use PG roles to assign actions and PG meta for city&program filters
  cUsr' <- with authDb $ replaceRolesFromPG cUsr
  Just meta <- with authDb $ userMetaPG cUsr
  logdUsers <- map (userLogin.snd) . Map.elems <$> addToLoggedUsers cUsr'

  actIds1 <- withPG pg_actass (`query_` assignQ 1 cUsr' meta logdUsers)
  actIds2 <- withPG pg_actass (`query_` assignQ 2 cUsr' meta logdUsers)
  actIds3 <- withPG pg_actass (`query_` assignQ 3 cUsr' meta logdUsers)
  let actIds = actIds1 ++ actIds2 ++ actIds3

  let uLogin = T.encodeUtf8 $ userLogin cUsr'
  with db $ forM_ actIds $ \[actId] ->
      DB.update "action" actId
        $ Map.singleton "assignedTo" uLogin

  when (not $ null actIds) $ log Info $ fromString
    $ "New actions for " ++ show uLogin
    ++ ": " ++ show actIds

  selectActions (Just "0") (Just uLogin) Nothing Nothing Nothing
    >>= writeJSON
