
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


assignQ :: Int -> AuthUser -> [Text] -> Query
assignQ pri usr logdUsers = fromString
  $  "UPDATE actiontbl SET assignedTo = '" ++ uLogin ++ "'"
  ++ "  WHERE id = (SELECT act.id"
  ++ "    FROM actiontbl act, servicetbl svc, casetbl c"
  ++ "    WHERE closed = false"
  ++ "    AND c.id::text = substring(act.caseId, ':(.*)')"
  ++ "    AND svc.id::text = substring(act.parentId, ':(.*)')"
  ++ "    AND svc.type::text = substring(act.parentId, '(.*):')"
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
    uRoles = intercalate "','" [B.unpack r | Role r <- userRoles usr]
    logdUsersList = T.unpack $ T.intercalate "','" logdUsers
    mkSet = T.unpack . T.intercalate "','" . T.split (==',')
    citySet = case HashMap.lookup "boCities" $ userMeta usr of
      Just (Aeson.String xx) | not $ T.null xx -> Just $ mkSet xx
      _ -> Nothing
    programSet = case HashMap.lookup "boPrograms" $ userMeta usr of
      Just (Aeson.String xx) | not $ T.null xx -> Just $ mkSet xx
      _ -> Nothing


littleMoreActionsHandler :: AppHandler ()
littleMoreActionsHandler = scoper "littleMoreActions" $ do
  Just cUsr <- with auth currentUser
  logdUsers <- map (userLogin.snd) . Map.elems <$> addToLoggedUsers cUsr

  actIds1 <- withPG pg_actass (`query_` assignQ 1 cUsr logdUsers)
  actIds2 <- withPG pg_actass (`query_` assignQ 2 cUsr logdUsers)
  actIds3 <- withPG pg_actass (`query_` assignQ 3 cUsr logdUsers)
  let actIds = actIds1 ++ actIds2 ++ actIds3

  let uLogin = T.encodeUtf8 $ userLogin cUsr
  with db $ forM_ actIds $ \[actId] ->
      DB.update "action" actId
        $ Map.singleton "assignedTo" uLogin

  when (not $ null actIds) $ log Info $ fromString
    $ "New actions for " ++ show uLogin
    ++ ": " ++ show actIds

  selectActions (Just "0") (Just uLogin) Nothing Nothing Nothing
    >>= writeJSON
